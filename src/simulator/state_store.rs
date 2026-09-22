use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::parsers::expr::Expression;
use crate::parsers::gates::DriveStrength;
use crate::register::{Register, REAL_WIDTH, X};
use crate::simulator::exec::ResolvedTarget;
use crate::simulator::gates::Strength;
use crate::simulator::program::FunctionDefinition;
use crate::simulator::tasks::Output;

/// What the `$random` stream starts from.
///
/// Zero is what iverilog's unseeded `$random` starts from, and
/// [`uniform`] maps it to a stream of its own — so a design that draws random
/// stimulus draws the *same* stimulus on every run, and the same stimulus
/// iverilog draws. A simulation gets a fresh [`StateStore`], so the stream
/// restarts here every time a design is set up.
const DEFAULT_RANDOM_SEED: i32 = 0;

/// How deep calls to a design's own functions may nest.
///
/// Every call gets a frame of its own, so a recursive function *works* — but it
/// works on the host's stack, and one that never reaches its base case would
/// take the process down with it. This is the bound that makes runaway
/// recursion a named error instead.
///
/// It is deliberately well under what the stack holds, and the margin has to
/// be generous: the earlier bound of 64 was measured against a *plain* chain of
/// calls, and a recursion reached from the continuous-assignment fixpoint —
/// `assign y = fact(n);` settled with `n` undriven — overflows a 2 MiB test
/// thread well before 64. A guard whose whole job is to prevent a stack
/// overflow must fire with room to spare, so this is set from the depth that
/// survives *that* path rather than the cheapest one.
pub const MAX_CALL_DEPTH: usize = 32;

/// The stream `$random` draws from.
///
/// The whole of its state is one 32 bit seed, because that is all IEEE
/// 1364-2005's reference generator has — see [`random_from_seed`].
/// [`eval`](crate::simulator::eval::eval) is handed a `&StateStore` and nothing
/// else, so the one system function that is not a pure function of its
/// arguments has to advance its state through a shared reference — hence the
/// [`Cell`]. Cloning a store clones the stream's position with it, so a
/// snapshot replays the same numbers.
#[derive(Clone, Debug)]
pub struct RandomStream(Cell<i32>);

impl Default for RandomStream {
    fn default() -> Self {
        RandomStream(Cell::new(DEFAULT_RANDOM_SEED))
    }
}

/// One draw of IEEE 1364-2005 17.9.3's `$random`: the value, and the seed the
/// next draw starts from.
///
/// The algorithm is the standard's own reference C, transcribed — including
/// its `float`-flavoured scaling and its truncation toward zero — because the
/// point of it is that every simulator draws the *same* numbers from the same
/// seed. Corpus `pr995` prints thirty-one seed/value pairs for each of two
/// seeds and `pr556` prints two hundred and fifty-six draws of the unseeded
/// stream, so any departure at all shows up immediately.
///
/// This is `rtl_dist_uniform(seed, INT32_MIN, INT32_MAX)`, whose bounds are
/// the whole of `integer` — which is why only the last of that function's
/// three branches is here.
pub fn random_from_seed(seed: i32) -> (i32, i32) {
    let (value, next) = uniform(seed, i32::MIN, i32::MAX);
    let scaled = (value + 2147483648.0) / 4294967295.0;
    let scaled = scaled * 4294967296.0 - 2147483648.0;
    // The standard truncates toward zero and then steps a negative result one
    // further down, which is C's `(int)(r - 1)` — a floor, spelled the long
    // way round.
    let drawn = if scaled >= 0.0 {
        scaled as i32
    } else {
        (scaled - 1.0) as i32
    };
    (drawn, next)
}

/// The generator underneath [`random_from_seed`]: a linear congruential step
/// on the seed, scaled into `start..=end`.
///
/// A seed of zero is replaced rather than used — an LCG started from zero is
/// still a perfectly good stream here, but the standard says to start
/// elsewhere and that constant is part of what makes an unseeded `$random`
/// reproducible across simulators.
fn uniform(seed: i32, start: i32, end: i32) -> (f64, i32) {
    let old = if seed == 0 { 259341593u32 } else { seed as u32 };
    let (a, b) = if start >= end {
        (0.0, 2147483647.0)
    } else {
        (f64::from(start), f64::from(end))
    };
    let next = 69069u32.wrapping_mul(old).wrapping_add(1);
    // 2^-23: the standard scales the seed's top bits as though they were the
    // mantissa of a `float` between 1.0 and 2.0.
    const D: f64 = 0.000_000_119_209_289_550_781_25;
    let mut c = 1.0 + f64::from(next >> 9) * D;
    c += c * D;
    c = ((b - a) * (c - 1.0)) + a;
    (c, next as i32)
}

/// Bit 31 of a descriptor marks a *file* descriptor rather than a
/// multi-channel one, which is what tells `$fopen(name, "w")`'s answer from
/// `$fopen(name)`'s.
const FILE_DESCRIPTOR: u32 = 1 << 31;

/// The highest multi-channel descriptor bit [`StateStore::open_channel`] hands
/// out. Bit 31 means something else, so a channel cannot live there.
const MAX_CHANNEL_BIT: u32 = 30;

/// The file descriptors already spoken for: 0 is standard input, 1 standard
/// output and 2 standard error, so the first one `$fopen` allocates is 3 —
/// which is what iverilog 12.0 hands back.
const FIRST_FILE_DESCRIPTOR: usize = 3;

/// Standard output, as a file descriptor.
const STDOUT_DESCRIPTOR: usize = 1;

/// The files a design has open, and where a relative path is written.
///
/// This lives on the [`StateStore`] for the same reason the `$random` stream
/// does: `$fopen` is a system *function*, so it is evaluated by
/// [`eval`](crate::simulator::eval::eval), which is handed a `&StateStore` and
/// nothing else. Hence the [`RefCell`] — opening a file has to be a
/// shared-reference operation. The [`Rc`] is what shares the table with a
/// function call's frame, so a file opened inside a `function` is a file the
/// design has open rather than one thrown away with the frame.
#[derive(Clone, Debug, Default)]
pub struct FileTable(Rc<RefCell<OpenFiles>>);

/// A file open for reading, and the one byte a look-ahead has taken off it.
///
/// The pushback slot is the whole of the reading model: `peek` reads a byte and
/// leaves it here, `bump` consumes it, and `$ungetc` puts one back by hand. A
/// scan that stops at a character it does not want therefore leaves the stream
/// positioned *before* it, which is what makes `$ftell` after a `$fscanf`
/// report what C reports.
#[derive(Debug)]
pub struct Reader {
    file: BufReader<File>,
    /// The byte `peek` has looked at and `bump` has not yet consumed.
    pushback: Option<u8>,
    /// Whether a read has run off the end. Sticky, like C's `feof`: it is set
    /// by a read that found nothing and cleared only by a seek.
    eof: bool,
}

impl Reader {
    /// The next byte without consuming it. `None` is end of file, and it is
    /// what sets the `$feof` flag — the same rule C follows, where a read that
    /// finds nothing is what makes `feof` true rather than merely being at the
    /// last byte.
    pub fn peek(&mut self) -> Option<u8> {
        if self.pushback.is_none() {
            let mut byte = [0u8; 1];
            match self.file.read(&mut byte) {
                Ok(1) => self.pushback = Some(byte[0]),
                _ => {
                    self.eof = true;
                    return None;
                }
            }
        }
        self.pushback
    }

    /// Consumes the byte [`peek`](Reader::peek) looked at.
    pub fn bump(&mut self) {
        self.pushback = None;
    }

    /// `$fgetc` — the next byte, consumed. `None` at end of file.
    pub fn take(&mut self) -> Option<u8> {
        let byte = self.peek();
        if byte.is_some() {
            self.bump();
        }
        byte
    }

    /// `$ungetc` — puts a byte back so the next read finds it. There is room
    /// for exactly one, which is all C promises; a second in a row fails.
    pub fn unread(&mut self, byte: u8) -> bool {
        if self.pushback.is_some() {
            return false;
        }
        self.pushback = Some(byte);
        self.eof = false;
        true
    }

    /// `$feof` — whether a read has run off the end.
    pub fn at_eof(&mut self) -> bool {
        self.eof
    }

    /// `$ftell` — the byte offset of the next character. A byte held in the
    /// pushback slot has been read off the underlying file but not consumed by
    /// the design, so it is counted back off.
    pub fn position(&mut self) -> Option<i64> {
        let position = self.file.stream_position().ok()?;
        let held = u64::from(self.pushback.is_some());
        i64::try_from(position.saturating_sub(held)).ok()
    }

    /// `$fseek` / `$rewind`. Seeking discards the look-ahead byte and clears
    /// the end-of-file flag, exactly as C's `fseek` does.
    pub fn seek(&mut self, to: SeekFrom) -> bool {
        self.pushback = None;
        match self.file.seek(to) {
            Ok(_) => {
                self.eof = false;
                true
            }
            Err(_) => false,
        }
    }
}

/// One open file: the direction it was opened in decides what it can do.
#[derive(Debug)]
enum Handle {
    /// Open for writing. `updates` records a mode with a `+` in it, which asks
    /// for a file that reads *and* writes — this simulator buffers one
    /// direction at a time, so a read of one of those is a named error rather
    /// than a silent end of file.
    Writing {
        writer: BufWriter<File>,
        updates: bool,
    },
    /// Open for reading.
    Reading(Reader),
}

/// One open file, or the slot a closed one left behind.
type Channel = Option<Handle>;

#[derive(Debug, Default)]
struct OpenFiles {
    /// The multi-channel descriptors, indexed by their bit. Bit 0 is standard
    /// output and never a file, so slot 0 stays empty.
    channels: Vec<Channel>,
    /// The file descriptors, indexed by their number. The first three are
    /// the standard streams and are never files either.
    descriptors: Vec<Channel>,
    /// Where a relative path is resolved. `None` is the process working
    /// directory, which is what a caller that never said otherwise gets.
    directory: Option<PathBuf>,
    /// Where a relative path is *read* from, after the process working
    /// directory. The mirror of `directory`, and the same list
    /// `$readmemh` searches — a design that opens a data file for reading and
    /// one that loads a memory from it are naming the same file.
    search_paths: Vec<PathBuf>,
}

impl OpenFiles {
    /// The first free slot at or after `first`, growing the list when every one
    /// already in it is taken. `None` once `last` is in use as well.
    fn free_slot(slots: &mut Vec<Channel>, first: usize, last: usize) -> Option<usize> {
        while slots.len() <= first {
            slots.push(None);
        }
        if let Some(index) = slots.iter().skip(first).position(Option::is_none) {
            return Some(first + index);
        }
        if slots.len() > last {
            return None;
        }
        slots.push(None);
        Some(slots.len() - 1)
    }

    /// Every slot the descriptor names that really is an open *writable* file:
    /// one for a file descriptor, and one per set bit for a channel mask.
    fn named(&mut self, descriptor: u32) -> Vec<&mut BufWriter<File>> {
        if descriptor & FILE_DESCRIPTOR != 0 {
            let index = (descriptor & !FILE_DESCRIPTOR) as usize;
            return self
                .descriptors
                .get_mut(index)
                .and_then(Option::as_mut)
                .and_then(Handle::writer)
                .into_iter()
                .collect();
        }
        self.channels
            .iter_mut()
            .enumerate()
            .filter(|(bit, _)| *bit >= 1 && descriptor & (1 << bit) != 0)
            .filter_map(|(_, slot)| slot.as_mut().and_then(Handle::writer))
            .collect()
    }

    /// Every open file, whichever kind of descriptor named it — what a
    /// `$fflush` with no argument flushes.
    fn all(&mut self) -> Vec<&mut BufWriter<File>> {
        self.channels
            .iter_mut()
            .chain(self.descriptors.iter_mut())
            .filter_map(|slot| slot.as_mut().and_then(Handle::writer))
            .collect()
    }
}

impl Handle {
    fn writer(&mut self) -> Option<&mut BufWriter<File>> {
        match self {
            Handle::Writing { writer, .. } => Some(writer),
            Handle::Reading(_) => None,
        }
    }
}

/// Why a descriptor cannot be read from.
///
/// The distinctions matter because a design *observes* the answer: iverilog
/// hands a `$fscanf` on a write-only file the same `-1` it hands one at end of
/// file, and prints a diagnostic for a descriptor that names nothing at all.
/// There is no diagnostic channel here, so the two that a design can act on are
/// reported as `-1` and the one that is genuinely unimplemented is a named
/// error.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NotReadable {
    /// A multi-channel descriptor, a descriptor nothing has open, or a file
    /// opened for writing. Each one reads as end of file, which is what
    /// iverilog returns for all three.
    EndOfFile,
    /// A file opened in an update mode (`r+`, `w+`, `a+`), which asks for a
    /// handle that reads and writes at once. This simulator buffers one
    /// direction at a time, so reading one is a named error rather than a
    /// silent end of file.
    Update,
}

/// The declared range of a `real`, which is what a sixty-four bit value's
/// range always is. A `real` has no declarable width of its own — the type is
/// the whole of it — so this is a constant rather than something parsed.
pub const REAL_RANGE: (i64, i64) = (REAL_WIDTH as i64 - 1, 0);

/// A single named signal: its current four-state value plus the `(msb, lsb)`
/// range it was declared with.
///
/// The range matters for bit and part selects: `reg [7:0] a` and `reg [0:7] a`
/// hold the same bits but `a[0]` names opposite ends of the vector.
///
/// Signedness lives here too, because in Verilog it is a property of the
/// *declaration* — `reg signed [3:0] a` — and not of the bits. It is carried on
/// the stored value itself rather than in a field beside it, so there is one
/// copy of it and an expression that reads the signal reads it along with the
/// bits; what makes it a *declared* property is that the store re-stamps it on
/// every write, and a value cannot bring its own.
/// The drivers of one bit of a net, tallied by what each was driving.
///
/// A driver that is contributing `z` — one that has let go, or whose `highz`
/// half is the one that applies — is **not** counted at all, which is what
/// makes `$countdrivers` of an undriven net `0` rather than the number of
/// `assign` statements naming it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DriverTally {
    pub zero: u32,
    pub one: u32,
    pub unknown: u32,
}

impl DriverTally {
    /// Counts one more driver, given the bit it is contributing.
    pub fn count(&mut self, code: u8) {
        match code {
            crate::register::ZERO => self.zero += 1,
            crate::register::ONE => self.one += 1,
            crate::register::X => self.unknown += 1,
            _ => {}
        }
    }

    /// Every driver that is driving something.
    pub fn total(&self) -> u32 {
        self.zero + self.one + self.unknown
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignalState {
    register: Register,
    range: (i64, i64),
    /// Whether the declaration was a *net* rather than a variable. Nothing in
    /// the run loop reads it — an undriven net's `z` is already in the value —
    /// but a waveform dump has to say `$var wire` where a `reg` says `$var
    /// reg`, and by then the declaration is long gone. It rides here for the
    /// same reason signedness does: the declaration is the only thing that
    /// knows, and it is set once and re-stamped rather than re-derived.
    net: bool,
    /// How many drivers reached each bit at the last resolution, most
    /// significant first — what `$countdrivers` reports. `None` unless the
    /// design calls it, which is what keeps the tally off every other
    /// design's propagation pass.
    drivers: Option<Vec<DriverTally>>,
    /// The strength each bit was last *resolved* at, most significant first.
    ///
    /// `None` for everything but a net that goes through
    /// `Simulator::resolve_contributions`, which is the only thing that knows
    /// one — an ordinary `assign` or a procedural write drives at `strong` and
    /// needs nothing recorded. So a design with no gate, no `tran` and no
    /// strength-bearing `assign` in it never allocates here, and `%v` answers
    /// from the value the way it always did.
    strengths: Option<Vec<Strength>>,
}

impl SignalState {
    /// Wraps a value with the implicit range `(width - 1, 0)`.
    pub fn new(register: Register) -> Self {
        let range = (register.width() as i64 - 1, 0);
        SignalState {
            register,
            range,
            net: false,
            drivers: None,
            strengths: None,
        }
    }

    /// Wraps a value with an explicit `(msb, lsb)` range.
    ///
    /// The range's bit count must match the register's width.
    pub fn with_range(register: Register, range: (i64, i64)) -> Self {
        assert_eq!(
            range_width(range),
            register.width(),
            "declared range {:?} does not match register width {}",
            range,
            register.width()
        );
        SignalState {
            register,
            range,
            net: false,
            drivers: None,
            strengths: None,
        }
    }

    /// The same signal, declared as a net rather than as a variable.
    pub fn as_net(mut self, net: bool) -> Self {
        self.net = net;
        self
    }

    /// Whether the signal was declared as a net — a `wire`, a `tri` or a port
    /// backed by one.
    pub fn is_net(&self) -> bool {
        self.net
    }

    /// The strength each bit was last resolved at, or `None` for a signal
    /// nothing resolves.
    pub fn strengths(&self) -> Option<&[Strength]> {
        self.strengths.as_deref()
    }

    /// How many drivers reached each bit at the last resolution, or `None` for
    /// a design that never asked.
    pub fn driver_counts(&self) -> Option<&[DriverTally]> {
        self.drivers.as_deref()
    }

    /// The same signal, declared signed or unsigned.
    pub fn with_signedness(mut self, signed: bool) -> Self {
        self.register = self.register.with_signedness(signed);
        self
    }

    /// Whether the signal was declared `signed`.
    pub fn is_signed(&self) -> bool {
        self.register.is_signed()
    }

    /// Whether the signal was declared `real`, which is what says its bits are
    /// a double. It rides on the stored value for the same reason signedness
    /// does: there is one copy of it, and the store re-stamps it on every
    /// write so a value cannot bring its own.
    pub fn is_real(&self) -> bool {
        self.register.is_real()
    }

    pub fn register(&self) -> &Register {
        &self.register
    }

    pub fn range(&self) -> (i64, i64) {
        self.range
    }

    pub fn width(&self) -> usize {
        self.register.width()
    }

    /// Translates a Verilog bit index into an offset into the register's
    /// most-significant-first bit vector, or `None` if the index is outside the
    /// declared range.
    pub fn bit_position(&self, index: i64) -> Option<usize> {
        bit_position_in(self.range, index)
    }

    /// The value of a single declared bit. Reading outside the declared range
    /// yields `x`, which is what Verilog does for an out-of-bounds select.
    pub fn bit(&self, index: i64) -> u8 {
        match self.bit_position(index) {
            // `bit_position` counts from the most significant end, the way the
            // bits are written in Verilog source; `Register` indexes from the
            // least significant end.
            Some(offset) => self
                .register
                .bit_from_lsb(self.width() - 1 - offset)
                .unwrap_or(X),
            None => X,
        }
    }

    /// Writes a single declared bit and reports whether the stored value moved.
    /// Writing outside the declared range is discarded, which is what Verilog
    /// does with an out-of-bounds select on the left of an assignment.
    pub fn set_bit(&mut self, index: i64, value: u8) -> bool {
        let Some(offset) = self.bit_position(index) else {
            return false;
        };
        let from_lsb = self.width() - 1 - offset;
        if self.register.bit_from_lsb(from_lsb) == Some(value) {
            return false;
        }
        // `with_bit` builds a fresh register, which is an unsigned one until it
        // is told otherwise; the declaration outlives a single bit write.
        let signed = self.register.is_signed();
        self.register = self
            .register
            .with_bit(from_lsb, value)
            .with_signedness(signed);
        true
    }
}

fn range_width(range: (i64, i64)) -> usize {
    ((range.0 - range.1).abs() + 1) as usize
}

/// A memory: `reg [7:0] mem [0:255];` — an array of words, each one a register
/// of its own.
///
/// A memory is kept in a map of its own rather than as a wider [`SignalState`],
/// and that separation is the whole disambiguation between a *bit* select and a
/// *word* select. `a[3]` and `m[3]` are the same syntax; which one is meant
/// depends only on how the name was declared, and the declaration reaches
/// [`eval`](crate::simulator::eval::eval) as *which map the name landed in*.
/// A name is a signal or a memory, never both.
///
/// A word that has never been written reads `x`, exactly like an undriven
/// register, and an address outside the declared range reads `x` and swallows a
/// write — the same thing [`SignalState::bit`] and [`SignalState::set_bit`] do
/// with an out-of-range bit.
///
/// An array may have more than one dimension — `reg [7:0] a [0:3][0:15];`, a
/// word of which is named `a[i][j]` — so the addresses are a *list* and the
/// words are laid out in row-major order, last dimension fastest. An address
/// is therefore always as many indices as the declaration wrote, which is what
/// keeps a partial one (`a[i]` of a two-dimensional array) a named error
/// rather than a word nothing named.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Memory {
    words: Vec<Register>,
    /// The declared address ranges, outermost first: `[(0, 255)]` for
    /// `mem [0:255]`, `[(0, 3), (0, 15)]` for `mem [0:3][0:15]`.
    addresses: Vec<(i64, i64)>,
    /// The `(msb, lsb)` range of one word.
    range: (i64, i64),
}

/// Where a declared bit index sits in a vector declared over `range`, counted
/// from the *most significant* end — the order Verilog writes bits in.
///
/// `None` is an index outside the declared range, which reads `x` and
/// discards a write. It is a free function rather than a method because a
/// memory word is a bare [`Register`] with no range of its own: the mapping
/// belongs to the declaration, and there are two kinds of declaration that
/// carry one.
pub fn bit_position_in(range: (i64, i64), index: i64) -> Option<usize> {
    let (msb, lsb) = range;
    let offset = if msb >= lsb {
        if index > msb || index < lsb {
            return None;
        }
        msb - index
    } else {
        if index < msb || index > lsb {
            return None;
        }
        index - msb
    };
    Some(offset as usize)
}

/// How many words an array over `addresses` holds — the product of its
/// dimensions, which is what a multi-dimensional declaration has to be
/// measured by.
/// The product saturates rather than wrapping: a wrapped one could come out
/// small enough to pass the depth guard and then allocate everything the
/// machine has.
pub fn array_depth(addresses: &[(i64, i64)]) -> usize {
    addresses.iter().fold(1usize, |depth, range| {
        depth.saturating_mul(range_width(*range))
    })
}

impl Memory {
    /// A memory of `addresses` words, each `range` wide and every bit `x`.
    pub fn new(addresses: Vec<(i64, i64)>, range: (i64, i64), signed: bool) -> Self {
        Memory::filled(addresses, range, signed, Register::unknown)
    }

    /// An **array of nets**, whose undriven words read `z` rather than `x` for
    /// the same reason a scalar net does — see [`StateStore::declare_net`].
    pub fn of_nets(addresses: Vec<(i64, i64)>, range: (i64, i64), signed: bool) -> Self {
        Memory::filled(addresses, range, signed, Register::high_impedance)
    }

    /// An array of `real`s. A real has no `x`, so an unwritten word is `0.0`
    /// rather than unknown — the same rule a scalar `real` follows.
    pub fn of_reals(addresses: Vec<(i64, i64)>) -> Self {
        Memory::filled(addresses, REAL_RANGE, true, |_| Register::from_f64(0.0))
    }

    fn filled(
        addresses: Vec<(i64, i64)>,
        range: (i64, i64),
        signed: bool,
        fill: impl Fn(usize) -> Register,
    ) -> Self {
        let word = fill(range_width(range)).with_signedness(signed);
        Memory {
            words: vec![word; array_depth(&addresses)],
            addresses,
            range,
        }
    }

    /// How many words the memory holds.
    pub fn depth(&self) -> usize {
        self.words.len()
    }

    /// The declared address ranges, outermost first.
    pub fn addresses(&self) -> &[(i64, i64)] {
        &self.addresses
    }

    /// How many address dimensions the declaration wrote, which is how many
    /// indices a word of this array is named by.
    pub fn dimensions(&self) -> usize {
        self.addresses.len()
    }

    /// The `(msb, lsb)` range of one word.
    pub fn range(&self) -> (i64, i64) {
        self.range
    }

    /// How wide one word is.
    pub fn width(&self) -> usize {
        range_width(self.range)
    }

    /// Whether the declaration carried a `signed` qualifier.
    pub fn is_signed(&self) -> bool {
        self.words[0].is_signed()
    }

    /// Whether this is an array of `real`s.
    pub fn is_real(&self) -> bool {
        self.words[0].is_real()
    }

    /// Translates a declared address into an offset into `words`, counting from
    /// the address written first — so `mem [0:255]` and `mem [15:8]` both run in
    /// the order their declarations read. `None` is an address outside the
    /// declared range.
    ///
    /// A multi-dimensional array takes one index per dimension and lays its
    /// words out row-major, last dimension fastest. An address with the wrong
    /// number of indices names no word at all, which is what keeps `a[i]` of a
    /// two-dimensional array from reading the first row's first word.
    pub fn word_position(&self, address: &[i64]) -> Option<usize> {
        if address.len() != self.addresses.len() {
            return None;
        }
        let mut offset = 0usize;
        for (index, (first, last)) in address.iter().zip(self.addresses.iter()) {
            let (first, last) = (*first, *last);
            let within = if first <= last {
                if *index < first || *index > last {
                    return None;
                }
                index - first
            } else {
                if *index > first || *index < last {
                    return None;
                }
                first - index
            };
            offset = offset * range_width((first, last)) + within as usize;
        }
        Some(offset)
    }

    /// The word at `address`. An address that is unknown — `None`, which is what
    /// an `x` index evaluates to — or outside the declared range reads `x`.
    pub fn word(&self, address: Option<&[i64]>) -> Register {
        match address.and_then(|address| self.word_position(address)) {
            Some(offset) => self.words[offset].clone(),
            // An array of reals has no unknown to read: `0.0` is what an
            // unwritten word of one holds, so it is what an unreachable one
            // reads too.
            None if self.is_real() => Register::from_f64(0.0),
            None => Register::unknown(self.width()).with_signedness(self.is_signed()),
        }
    }

    /// The value of one declared bit of a word already read out of this
    /// memory: the second bracket of `mem[a][i]`.
    ///
    /// It takes the word rather than the address so a part select costs one
    /// read of the memory rather than one per bit.
    pub fn bit_of(&self, word: &Register, index: i64) -> u8 {
        match bit_position_in(self.range, index) {
            Some(offset) => word.bit_from_lsb(self.width() - 1 - offset).unwrap_or(X),
            None => X,
        }
    }

    /// The same word with one declared bit replaced. A bit outside the declared
    /// range is dropped, which is what an out-of-range write already does.
    pub fn with_bit_of(&self, word: Register, index: i64, code: u8) -> Register {
        match bit_position_in(self.range, index) {
            Some(offset) => word.with_bit(self.width() - 1 - offset, code),
            None => word,
        }
    }

    /// Writes a word, resized to the memory's own width, and reports whether the
    /// stored value moved. A write outside the declared range is discarded.
    pub fn set_word(&mut self, address: &[i64], value: &Register) -> bool {
        let Some(offset) = self.word_position(address) else {
            return false;
        };
        // Signedness is the *declaration's*, so it is re-stamped on every write
        // exactly as `SignalState` does it: a value cannot bring its own. So is
        // realness, and a word of an array of reals is *converted* rather than
        // re-stamped: the bits of a double are not the bits of the integer that
        // denotes the same number.
        let signed = self.words[offset].is_signed();
        let value = if self.words[offset].is_real() {
            Register::from_f64(value.to_f64())
        } else {
            value.coerced(self.width()).with_signedness(signed)
        };
        if self.words[offset] == value {
            return false;
        }
        self.words[offset] = value;
        true
    }
}

/// How strongly something drives a signal.
///
/// Verilog gives a variable more than one potential source, and says which one
/// wins: a `force` beats a procedural continuous `assign`, which beats an
/// ordinary procedural write. The order of these variants *is* that rule —
/// `exec::held_bits` compares them — so keep them written weakest first.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DriveLevel {
    /// An ordinary write: a blocking or non-blocking assignment, a module-level
    /// continuous assignment, a testbench driving an input. Everything that is
    /// not one of the two below.
    Procedural,
    /// A procedural continuous assignment — `assign v = e;` inside a block.
    Assign,
    /// A `force`.
    Force,
}

/// A continuous drive a procedural block installed with `assign` or `force`.
///
/// It is *continuous*: the value is not written once when the statement runs,
/// it is re-evaluated whenever anything the design does could have moved one of
/// its operands. The drive therefore has to outlive the statement that
/// installed it, and it lives here rather than on the `Simulator` because the
/// only thing a running procedural block is handed is a [`StateStore`].
#[derive(Clone, Debug, PartialEq)]
pub struct Drive {
    /// The signals the target names, which are the keys the precedence rule is
    /// answered by. A concatenation target — `assign {a, b} = e;` — names
    /// several, and the drive holds every one of them, so that a write to any
    /// of them finds it.
    names: Vec<String>,
    /// The left hand side, kept unresolved so that the drive re-resolves it the
    /// way a module-level continuous assignment does — a variable index in
    /// `force m[i] = e;` follows `i`.
    target: Expression,
    value: Expression,
    level: DriveLevel,
}

impl Drive {
    pub fn new(
        names: Vec<String>,
        target: Expression,
        value: Expression,
        level: DriveLevel,
    ) -> Self {
        Drive {
            names,
            target,
            value,
            level,
        }
    }

    /// Whether this drive holds any part of `name`.
    pub fn covers(&self, name: &str) -> bool {
        self.names.iter().any(|held| held == name)
    }

    /// Every signal this drive holds part of.
    pub fn names(&self) -> &[String] {
        &self.names
    }

    pub fn target(&self) -> &Expression {
        &self.target
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }

    pub fn level(&self) -> DriveLevel {
        self.level
    }
}

/// One word of a memory that moved since the last marker: which memory, which
/// word, what the round first displaced and what it last wrote.
///
/// The word is its flat position in the memory — [`Memory::word_position`] of
/// the address it was written at — so an array of any number of dimensions
/// keys one word by one number.
#[derive(Clone, Debug, PartialEq)]
pub struct MemoryChange {
    pub name: String,
    pub position: usize,
    pub before: Register,
    pub after: Register,
}

/// Name to value map for every signal in a simulation, together with a journal
/// of everything written since the last marker.
///
/// The journal is what makes edge detection affordable. A scheduler that has to
/// discover which signals moved by diffing two whole snapshots pays for every
/// signal in the design on every delta cycle, when the set of signals that
/// could possibly have moved is exactly the set something wrote. Writes record
/// the value they displaced; [`take_changes`](StateStore::take_changes) hands
/// that list over and starts a fresh one.
/// The store also carries the simulation context an expression can read but no
/// signal holds — the current time and the `$random` stream — because a
/// `&StateStore` is all [`eval`](crate::simulator::eval::eval) is given.
/// The declarations one scope made, as [`StateStore::scope_storage`] took them.
#[derive(Clone, Debug, Default)]
pub struct ScopeStorage {
    signals: Vec<(String, SignalState)>,
    memories: Vec<(String, Memory)>,
    events: Vec<String>,
}

#[derive(Clone, Debug, Default)]
pub struct StateStore {
    name_to_signal: HashMap<String, SignalState>,
    /// Whether the design calls `$countdrivers` anywhere. It is what turns the
    /// driver tally on, so a design that never asks never builds one.
    counts_drivers: bool,
    /// A port bound to a plain identifier is *one* store entry with the
    /// parent's signal, so the port's own qualified name has no entry — and a
    /// testbench that reaches into the instance by that name has to find it.
    /// Shared with the `Simulator` rather than copied, the way the file table
    /// and the `$random` stream are.
    aliases: Rc<HashMap<String, String>>,
    /// The memories the design declares, keyed by qualified name.
    ///
    /// Deliberately a second map rather than a field on [`SignalState`]: a
    /// memory needs `n` words where a signal needs one, and the lookup that
    /// every bit select goes through is the hot path. A name is in one map or
    /// the other, so an ordinary select still costs one hash and only a miss
    /// looks here.
    name_to_memory: HashMap<String, Memory>,
    /// Every memory word written since the last marker, in the order written.
    /// See [`set_word`](StateStore::set_word).
    ///
    /// A list that is only appended to, and merged per word when it is taken:
    /// a loop that fills a memory writes one entry per word, and looking each
    /// one up on the way in would make that loop quadratic. Taking an empty
    /// one costs nothing — which matters because every delta cycle asks.
    memory_journal: Vec<MemoryChange>,
    /// The words the settle round now running took out of `memory_journal`,
    /// which is where a sensitivity entry naming *one* word looks — see
    /// [`round_word`](StateStore::round_word). Kept here rather than on the
    /// edge list because an edge is a signal's, and widening every one of them
    /// by an address costs a design with no memory in it ~4.5% on
    /// `bench tick/counter_4bit`.
    round_words: Vec<MemoryChange>,
    /// For every signal written since the last marker, the value it held at
    /// that marker. `None` records a name that did not exist yet, which makes
    /// the write a declaration rather than a change.
    journal: HashMap<String, Option<Register>>,
    /// What `$time` reads. The driver moves it as simulated time moves.
    time: i64,
    random: RandomStream,
    /// The functions the design declares, keyed by qualified name.
    ///
    /// They live here because [`eval`](crate::simulator::eval::eval) is handed
    /// a `&StateStore` and nothing else, and a call has to find its definition.
    /// The table is shared rather than copied so that a call's frame — itself a
    /// store — can call a function in turn without cloning every compiled body.
    functions: Rc<HashMap<String, FunctionDefinition>>,
    /// The `+name=value` words the simulation was started with, without their
    /// `+`.
    ///
    /// Here for the reason the `$random` stream and the file table are:
    /// `$test$plusargs` and `$value$plusargs` are system *functions*, so
    /// [`eval`](crate::simulator::eval::eval) is the only thing that can read
    /// them and it is handed a `&StateStore` and nothing else. Shared rather
    /// than copied, and never written after `setup`, so a call's frame reads
    /// the same list the design does.
    plusargs: Rc<Vec<String>>,
    /// How many function calls are on the stack above this store.
    call_depth: Cell<usize>,
    /// Whether any signal here was declared signed.
    ///
    /// A hint, not a fact to reason from: it is set when a signed signal is
    /// declared and never cleared. It exists because `expression_is_signed`
    /// asks about every identifier it walks past, and hashing a name to answer
    /// "no" for a design that declares nothing signed is most of what that walk
    /// would otherwise cost.
    any_signed: bool,
    /// Whether any signal here was declared `real`, on the same terms as
    /// `any_signed`: a hint that is exact when it says `false`. It is what
    /// keeps the integer-to-real conversion every write would otherwise have
    /// to consider off a design that has no real in it.
    any_real: bool,
    /// Whether the design declares any memory at all, on the same terms as
    /// `any_signed`. `resolve_target` has to ask "is this name a memory?" of
    /// every bit-select write, and for a design with no memories this answers
    /// it without hashing the name.
    any_memory: bool,
    /// The named events the design declares: `event done;`.
    ///
    /// An event has no value, so it is deliberately not a signal — a third
    /// namespace beside the signals and the memories rather than a zero-width
    /// entry in either. That is what makes reading one a named error instead
    /// of a plausible pattern of bits.
    events: HashSet<String>,
    /// Every event triggered since the last marker, in trigger order.
    ///
    /// This is the whole of an event's state. A trigger is momentary: it is
    /// journalled here, turned into an edge by the settle loop, and gone. A
    /// list rather than a set because a design has a handful of events at
    /// most, and taking an empty one costs nothing.
    triggers: Vec<String>,
    /// The `force`s and procedural `assign`s currently installed, in the order
    /// they were installed.
    ///
    /// A `Vec` and not a map: a design has a handful of these at most and most
    /// have none, so the check every write makes is "is this list empty?" —
    /// a length compare — rather than a hash. Behind an [`Rc`] so that the
    /// driver can hold the list while writing through `&mut StateStore`,
    /// which is what lets these join the continuous-assignment fixpoint;
    /// installing one goes through [`Rc::make_mut`], the same way the function
    /// table does.
    drives: Rc<Vec<Drive>>,
    /// The right hand sides an intra-assignment timing control is holding on
    /// to, keyed by the hidden slot the instruction that evaluated them named.
    ///
    /// `a = @(posedge clk) b;` evaluates `b` when the statement runs and
    /// writes it when the edge arrives, and the block suspends in between — so
    /// the value has to outlive the return from `resume`, exactly as a
    /// `repeat` count does. It is deliberately not a signal: nothing in the
    /// design can name it, so journalling it would only manufacture edges.
    holds: HashMap<String, Register>,
    /// The files `$fopen` has opened, and the directory a relative path is
    /// written into. See [`FileTable`].
    files: FileTable,
    /// The writes a system *function* owes the design. See
    /// [`owe_fill`](StateStore::owe_fill).
    fills: RefCell<Vec<(ResolvedTarget, Register)>>,
    /// Where a `$display` written inside a *function* body prints.
    ///
    /// [`eval`](crate::simulator::eval::eval) is handed a `&StateStore` and
    /// nothing else, so this is the only route a function call has to the
    /// buffer the design prints into — the same reasoning that put the
    /// `$random` stream and the file table here.
    /// [`Simulator::setup`](crate::simulator::runner::Simulator::setup) hands
    /// over the handle its `TaskContext` prints into, and [`frame`](StateStore::frame)
    /// passes it down, so a nested call prints into the same string in the
    /// order the calls ran.
    ///
    /// A store nobody has linked keeps a buffer of its own, which is what
    /// makes a *constant* function evaluated while the design is still being
    /// elaborated print nothing — iverilog 12.0 drops that output too (corpus
    /// `constfunc13`).
    output: Output,
}

impl StateStore {
    pub fn new() -> Self {
        StateStore::default()
    }

    /// Whether any signal in the store was declared signed. `false` is exact —
    /// nothing here is signed — while `true` only means something once was.
    pub fn any_signed(&self) -> bool {
        self.any_signed
    }

    /// Whether any signal in the store was declared `real`. `false` is exact —
    /// nothing here is a real — while `true` only means something once was.
    pub fn any_real(&self) -> bool {
        self.any_real
    }

    /// A store for the body of a function called against this one: the same
    /// function table and the same clock, but no signals — a call's variables
    /// are its own, and nothing it writes is allowed to reach the design.
    ///
    /// The call depth comes across so that recursion is bounded by the whole
    /// chain of calls rather than restarting at every frame. The `$random`
    /// stream deliberately does not: a frame is thrown away, so advancing a
    /// stream in one would be lost, which is why a function body that draws
    /// from it is rejected when it is elaborated.
    pub fn frame(&self) -> StateStore {
        StateStore {
            name_to_signal: HashMap::new(),
            // A frame holds no net, so it resolves nothing and tallies
            // nothing; the alias table rides along because a body may read a
            // design signal under a port's name.
            counts_drivers: false,
            aliases: Rc::clone(&self.aliases),
            name_to_memory: HashMap::new(),
            memory_journal: Vec::new(),
            round_words: Vec::new(),
            journal: HashMap::new(),
            time: self.time,
            random: RandomStream::default(),
            functions: Rc::clone(&self.functions),
            plusargs: Rc::clone(&self.plusargs),
            call_depth: Cell::new(self.call_depth.get()),
            any_signed: false,
            any_real: false,
            any_memory: false,
            events: HashSet::new(),
            triggers: Vec::new(),
            // A frame holds only the call's own variables, and a function body
            // may not install a drive — nothing here can be forced.
            drives: Rc::new(Vec::new()),
            holds: HashMap::new(),
            // Shared, not fresh: `$fopen` is an expression, so it can be
            // called from a function body, and a file it opened there has to
            // outlive the frame the way a file opened anywhere else does.
            files: self.files.clone(),
            // Fresh, unlike the file table: a frame's writes are its own, so a
            // `$sscanf` inside a function body fills the function's variables
            // and nothing the design can see.
            fills: RefCell::new(Vec::new()),
            // Shared, like the file table: what a function body prints is the
            // design's output, and it has to land in it as it is printed.
            output: self.output.clone(),
        }
    }

    /// The buffer a `$display` inside a function body prints into.
    pub fn output(&self) -> &Output {
        &self.output
    }

    /// Points the store at the buffer the driver's `TaskContext` prints into,
    /// which is what makes a function body's output the design's output.
    pub fn print_into(&mut self, output: Output) {
        self.output = output;
    }

    /// Records a write a system *function* made through its argument list.
    ///
    /// `$sscanf` returns how many values it converted **and** writes them into
    /// the arguments it was handed, so it is the one kind of expression that
    /// changes the store — and [`eval`](crate::simulator::eval::eval) is given
    /// a `&StateStore`. The write is therefore queued here and carried out at
    /// the next instruction boundary by
    /// [`resume`](crate::simulator::program::resume), which is the first moment
    /// the statement that evaluated the expression has finished with it. Doing
    /// it there rather than inside `eval` is what keeps the assignment's own
    /// target (`code = $sscanf(…)`) written first, and what keeps the queue
    /// from having to know anything about assignment order.
    pub fn owe_fill(&self, target: ResolvedTarget, value: Register) {
        self.fills.borrow_mut().push((target, value));
    }

    /// Whether any system function has left a write outstanding. Asked once per
    /// instruction, so it is deliberately a length compare rather than
    /// anything that allocates.
    pub fn owes_fills(&self) -> bool {
        !self.fills.borrow().is_empty()
    }

    /// The value a queued write is going to put into `name`, if one is.
    ///
    /// A function that writes a design signal queues that write here, and two
    /// calls in one expression — `{ufunc(0), ufunc(0)}` — happen with nothing
    /// in between to drain the queue. So the second call seeds its frame from
    /// what the first one owes rather than from the signal, which is the only
    /// way the two answer differently (corpus `concat3`). The *last* entry
    /// wins, since the queue is in call order.
    pub fn pending_fill(&self, name: &str) -> Option<Register> {
        self.fills
            .borrow()
            .iter()
            .rev()
            .find_map(|(target, value)| match target {
                ResolvedTarget::Whole(written) if written == name => Some(value.clone()),
                _ => None,
            })
    }

    /// Hands the outstanding writes over and starts a fresh list.
    pub fn take_fills(&mut self) -> Vec<(ResolvedTarget, Register)> {
        std::mem::take(&mut self.fills.borrow_mut())
    }

    /// Where a relative `$fopen` or `$writememh` path is written, which
    /// defaults to the process working directory.
    ///
    /// A `Simulator` is built from parsed modules and never learns which file
    /// they came from, so it cannot put a design's output "next to the design"
    /// on its own — this is the seam a caller that does know uses, the write
    /// side of
    /// [`Simulator::add_search_path`](crate::simulator::runner::Simulator::add_search_path).
    pub fn set_output_directory(&mut self, directory: impl Into<PathBuf>) {
        self.files.0.borrow_mut().directory = Some(directory.into());
    }

    /// Where a relative `$fopen(name, "r")` path is looked for, after the
    /// process working directory.
    ///
    /// This is the read half of [`set_output_directory`](StateStore::set_output_directory)
    /// and it is the same list `$readmemh` searches — see
    /// [`Simulator::add_search_path`](crate::simulator::runner::Simulator::add_search_path).
    /// It lives here rather than on the `TaskContext` because `$fopen` is a
    /// system *function*, and `eval` is handed the store and nothing else.
    pub fn set_search_paths(&mut self, directories: Vec<PathBuf>) {
        self.files.0.borrow_mut().search_paths = directories;
    }

    /// The `+name=value` words the simulation was started with, without their
    /// `+`. See [`Simulator::add_plusarg`](crate::simulator::runner::Simulator::add_plusarg).
    pub fn set_plusargs(&mut self, plusargs: Vec<String>) {
        self.plusargs = Rc::new(plusargs);
    }

    /// What `$test$plusargs` and `$value$plusargs` read. An empty list is an
    /// ordinary answer — a design that finds no plus-arg is told so — not a
    /// design that has gone wrong.
    pub fn plusargs(&self) -> &[String] {
        &self.plusargs
    }

    /// The path a name denotes when it is being *read*: itself if it is
    /// absolute, otherwise the first of the process working directory and the
    /// search paths that has it. `None` names nothing that exists, and the
    /// caller says what that means.
    ///
    /// The output directory is asked first: a design that writes
    /// `work/temp.txt` and then opens `work/temp.txt` to read it back is naming
    /// one file, the way it would be under a simulator that runs in a single
    /// directory. Without that the read would miss the file the design has
    /// just written and quietly read nothing (corpus `pr1876798`).
    pub fn resolve_read_path(&self, name: &str) -> Option<PathBuf> {
        let written = self.resolve_write_path(name);
        if written.is_file() {
            return Some(written);
        }
        let path = Path::new(name);
        if path.is_file() {
            return Some(path.to_path_buf());
        }
        if path.is_absolute() {
            return None;
        }
        let files = self.files.0.borrow();
        files
            .search_paths
            .iter()
            .map(|directory| directory.join(path))
            .find(|candidate| candidate.is_file())
    }

    /// Every directory a relative read path is looked for in, which is what an
    /// error naming a file it could not find lists.
    pub fn search_paths(&self) -> Vec<PathBuf> {
        self.files.0.borrow().search_paths.clone()
    }

    /// The path a `$fopen` or `$writemem…` name denotes. An absolute one is
    /// itself; a relative one hangs off the output directory.
    pub fn resolve_write_path(&self, name: &str) -> PathBuf {
        let path = Path::new(name);
        if path.is_absolute() {
            return path.to_path_buf();
        }
        match &self.files.0.borrow().directory {
            Some(directory) => directory.join(path),
            None => path.to_path_buf(),
        }
    }

    /// `$fopen(name)` — opens `name` for writing and returns the multi-channel
    /// descriptor bit standing for it: 2, then 4, then 8, allocated from bit 1
    /// upwards because bit 0 is standard output.
    ///
    /// A file that cannot be opened is **0**, not an error, because 0 is what a
    /// design tests for — `if (fp == 0) $display("FAILED")` is how these tests
    /// are written, and an error would take the design's own report away from
    /// it.
    pub fn open_channel(&self, name: &str) -> u32 {
        let path = self.resolve_write_path(name);
        let mut files = self.files.0.borrow_mut();
        let Some(bit) = OpenFiles::free_slot(&mut files.channels, 1, MAX_CHANNEL_BIT as usize)
        else {
            return 0;
        };
        match File::create(&path) {
            Ok(file) => {
                files.channels[bit] = Some(Handle::Writing {
                    writer: BufWriter::new(file),
                    updates: false,
                });
                1 << bit
            }
            Err(_) => 0,
        }
    }

    /// `$fopen(name, mode)` — opens `name` in a C `fopen` mode and returns a
    /// *file* descriptor: bit 31 set, over a number allocated from 3 upwards.
    /// 0 on failure, exactly as [`open_channel`](StateStore::open_channel).
    /// A relative name in a **reading** mode is resolved against the search
    /// paths rather than the output directory — a design that opens a data
    /// file beside itself is naming the same file `$readmemh` would.
    pub fn open_descriptor(&self, name: &str, mode: &str) -> u32 {
        let mut options = OpenOptions::new();
        let mode = mode.trim_end_matches('b');
        let (reads, updates) = match mode {
            "r" => (true, false),
            "r+" => (true, true),
            "w" => (false, false),
            "w+" | "a+" => (false, true),
            "a" => (false, false),
            _ => return 0,
        };
        match mode {
            "r" => options.read(true),
            "r+" => options.read(true).write(true),
            "w" => options.write(true).create(true).truncate(true),
            "w+" => options.read(true).write(true).create(true).truncate(true),
            "a" => options.append(true).create(true),
            "a+" => options.read(true).append(true).create(true),
            _ => return 0,
        };
        // A file being opened to read has to exist already, so the search path
        // is what finds it; one being opened to write is being created, so
        // there is nothing to search for and it hangs off the output directory.
        let path = if reads {
            match self.resolve_read_path(name) {
                Some(path) => path,
                None => return 0,
            }
        } else {
            self.resolve_write_path(name)
        };
        let mut files = self.files.0.borrow_mut();
        let Some(index) = OpenFiles::free_slot(
            &mut files.descriptors,
            FIRST_FILE_DESCRIPTOR,
            (!FILE_DESCRIPTOR) as usize,
        ) else {
            return 0;
        };
        match options.open(&path) {
            Ok(file) => {
                files.descriptors[index] = Some(if reads && !updates {
                    Handle::Reading(Reader {
                        file: BufReader::new(file),
                        pushback: None,
                        eof: false,
                    })
                } else {
                    Handle::Writing {
                        writer: BufWriter::new(file),
                        updates,
                    }
                });
                FILE_DESCRIPTOR | index as u32
            }
            Err(_) => 0,
        }
    }

    /// Runs `action` against the file the descriptor names, open for reading.
    ///
    /// Access is by closure because the handle lives behind the table's
    /// [`RefCell`]: handing a borrow out would keep the table locked for as
    /// long as the caller held it, and `$fscanf` writes through the store while
    /// it reads.
    pub fn with_reader<T>(
        &self,
        descriptor: u32,
        action: impl FnOnce(&mut Reader) -> T,
    ) -> Result<T, NotReadable> {
        let mut files = self.files.0.borrow_mut();
        if descriptor & FILE_DESCRIPTOR == 0 {
            // A multi-channel descriptor is a write-only thing by
            // construction: `$fopen(name)` opens for writing.
            return Err(NotReadable::EndOfFile);
        }
        let index = (descriptor & !FILE_DESCRIPTOR) as usize;
        match files.descriptors.get_mut(index).and_then(Option::as_mut) {
            Some(Handle::Reading(reader)) => Ok(action(reader)),
            Some(Handle::Writing { updates: true, .. }) => Err(NotReadable::Update),
            Some(Handle::Writing { .. }) | None => Err(NotReadable::EndOfFile),
        }
    }

    /// Writes `text` to every open file the descriptor names, and reports
    /// whether **standard output** was among them — which is what makes
    /// `$fdisplay(fp|1, …)` reach the design's output buffer as well as its
    /// file.
    ///
    /// A bit naming a channel nothing opened is dropped, which is what iverilog
    /// does with one: it warns on standard error and carries on. Refusing the
    /// whole call instead would stop a design over a channel its output does
    /// not depend on.
    pub fn write_channels(&self, descriptor: u32, text: &str) -> bool {
        let mut files = self.files.0.borrow_mut();
        for file in files.named(descriptor) {
            let _ = file.write_all(text.as_bytes());
        }
        if descriptor & FILE_DESCRIPTOR != 0 {
            return (descriptor & !FILE_DESCRIPTOR) as usize == STDOUT_DESCRIPTOR;
        }
        descriptor & 1 != 0
    }

    /// `$fclose` — closes every file the descriptor names, freeing its bit for
    /// the next `$fopen`. Standard output is not a file, so `$fclose(1)` closes
    /// nothing.
    pub fn close_channels(&self, descriptor: u32) {
        let mut files = self.files.0.borrow_mut();
        if descriptor & FILE_DESCRIPTOR != 0 {
            let index = (descriptor & !FILE_DESCRIPTOR) as usize;
            if index >= FIRST_FILE_DESCRIPTOR {
                if let Some(slot) = files.descriptors.get_mut(index) {
                    *slot = None;
                }
            }
            return;
        }
        for bit in 1..=MAX_CHANNEL_BIT {
            if descriptor & (1 << bit) == 0 {
                continue;
            }
            if let Some(slot) = files.channels.get_mut(bit as usize) {
                *slot = None;
            }
        }
    }

    /// `$fflush` — pushes what is buffered out to the files the descriptor
    /// names, or to every open file when there is no descriptor at all, which
    /// is what a bare `$fflush;` means.
    pub fn flush_channels(&self, descriptor: Option<u32>) {
        let mut files = self.files.0.borrow_mut();
        let open = match descriptor {
            Some(descriptor) => files.named(descriptor),
            None => files.all(),
        };
        for file in open {
            let _ = file.flush();
        }
    }

    /// Whether anything at all is forced or procedurally assigned. `false` is
    /// exact, and is what keeps the drives off a design that uses none.
    pub fn has_drives(&self) -> bool {
        !self.drives.is_empty()
    }

    /// How many drives are installed, which is how many more rounds the
    /// continuous-assignment fixpoint may need.
    pub fn drive_count(&self) -> usize {
        self.drives.len()
    }

    /// The installed drives, as a handle the caller may hold while writing
    /// through the store — which is exactly what re-evaluating them needs.
    pub fn drives(&self) -> Rc<Vec<Drive>> {
        Rc::clone(&self.drives)
    }

    /// The drive of `level` holding any part of `name`, if there is one.
    pub fn drive(&self, name: &str, level: DriveLevel) -> Option<&Drive> {
        self.drives
            .iter()
            .find(|drive| drive.level == level && drive.covers(name))
    }

    /// Installs a drive, replacing one of the same strength on the same
    /// *target*.
    ///
    /// Re-`force`ing an already forced target replaces what it drives; there
    /// is nothing to remember, because a `release` puts nothing back. The key
    /// is the target rather than the signal, because a drive is per **bit**:
    /// `force bus[0]` and `force bus[3:2]` hold different parts of one vector
    /// and both stand at once. Keying by signal name made the second silently
    /// evict the first (corpus `force_release_reg_pv`, `assign_deassign_pv`).
    ///
    /// A new drive goes on the end, so where two of them do overlap the later
    /// one is applied last and wins — which is the rule the LRM asks for,
    /// falling out of the order rather than needing one of its own.
    pub fn install_drive(&mut self, drive: Drive) {
        let drives = Rc::make_mut(&mut self.drives);
        match drives.iter_mut().find(|existing| {
            existing.names == drive.names
                && existing.level == drive.level
                && existing.target == drive.target
        }) {
            Some(existing) => existing.value = drive.value,
            None => drives.push(drive),
        }
    }

    /// Keeps the drives `keep` marks, which is positional over [`drives`].
    ///
    /// Which drives a `release` takes off is a question about the *bits* each
    /// one holds, and resolving a target needs the evaluator — so the decision
    /// is made in `exec::release_drive`, and this applies it.
    ///
    /// [`drives`]: StateStore::drives
    pub fn retain_drives(&mut self, keep: &[bool]) {
        if keep.iter().all(|keep| *keep) {
            return;
        }
        let mut index = 0;
        Rc::make_mut(&mut self.drives).retain(|_| {
            let kept = keep.get(index).copied().unwrap_or(true);
            index += 1;
            kept
        });
    }

    /// Records a function the design declared, under its qualified name.
    pub fn declare_function(&mut self, name: impl Into<String>, definition: FunctionDefinition) {
        Rc::make_mut(&mut self.functions).insert(name.into(), definition);
    }

    /// The definition a call resolves against, if the design declares one.
    pub fn function(&self, name: &str) -> Option<&FunctionDefinition> {
        self.functions.get(name)
    }

    /// Every definition, to be rewritten in place.
    ///
    /// The one caller is elaboration's alias pass, which has to reach the names
    /// a function body holds for the reason it has to reach the ones a block
    /// holds — a hierarchical reference is legal in either. A frame shares the
    /// table by `Rc`, so this is the same `make_mut` a declaration goes
    /// through, and it runs before any frame exists.
    pub fn functions_mut(&mut self) -> &mut HashMap<String, FunctionDefinition> {
        Rc::make_mut(&mut self.functions)
    }

    /// Counts one more call onto the stack, or reports that the chain of calls
    /// has gone too deep to be anything but runaway recursion.
    ///
    /// The count comes back down when the guard is dropped, so an error on the
    /// way out of a call unwinds it exactly as a value does.
    pub fn enter_call(&self) -> Option<CallGuard<'_>> {
        if self.call_depth.get() >= MAX_CALL_DEPTH {
            return None;
        }
        self.call_depth.set(self.call_depth.get() + 1);
        Some(CallGuard(self))
    }

    /// The simulated time `$time` reports.
    pub fn time(&self) -> i64 {
        self.time
    }

    /// Tells the store what time it is. The driver does this whenever
    /// simulated time moves, so `$time` reads the timestamp the expression
    /// around it is evaluated at.
    pub fn set_time(&mut self, time: i64) {
        self.time = time;
    }

    /// The next number in the `$random` stream, as Verilog's 32 bit integer.
    ///
    /// This is the stream a bare `$random` draws from. `$random(seed)` keeps
    /// its stream in the design's own variable instead and so does not come
    /// here — see [`random_from_seed`].
    pub fn next_random(&self) -> i32 {
        let (value, next) = random_from_seed(self.random.0.get());
        self.random.0.set(next);
        value
    }

    /// Notes the value `name` holds right now, so that a write about to land on
    /// it can be reported as a transition.
    ///
    /// Only the first write since the marker is recorded: a later one would
    /// overwrite the value the round actually started from, and it is that
    /// value an edge has to be measured against.
    fn record(&mut self, name: &str) {
        if self.journal.contains_key(name) {
            return;
        }
        let previous = self
            .name_to_signal
            .get(name)
            .map(|signal| signal.register().clone());
        self.journal.insert(name.to_string(), previous);
    }

    /// The name and pre-write value of every signal written since the last
    /// call, sorted by name, clearing the journal so the next round is measured
    /// from here.
    ///
    /// A name that did not exist at the last call is left out: it was declared
    /// rather than changed, and declaring a signal is not a simulation event.
    /// Writes that put back the value already there are still reported — the
    /// journal records what was displaced, not whether it differed — so the
    /// caller compares.
    pub fn take_changes(&mut self) -> Vec<(String, Register)> {
        let mut changes = Vec::with_capacity(self.journal.len());
        for (name, previous) in self.journal.drain() {
            if let Some(previous) = previous {
                changes.push((name, previous));
            }
        }
        changes.sort_by(|left, right| left.0.cmp(&right.0));
        changes
    }

    /// Rewrites every journalled starting value that was entirely `z` as `x`.
    ///
    /// This is for the one moment before simulation starts, when a *driven*
    /// net holds `x` rather than the `z` of a net nothing drives. Its drivers'
    /// first values are then events only when they differ from `x` — which is
    /// the rule iverilog follows, and which a `z` baseline gets wrong in the
    /// direction of waking blocks on a value nobody set.
    pub fn treat_undriven_start_as_unknown(&mut self) {
        for previous in self.journal.values_mut().flatten() {
            if *previous == Register::high_impedance(previous.width()) {
                *previous = Register::unknown(previous.width());
            }
        }
    }

    /// Forgets every recorded change, making now the point later changes are
    /// measured against.
    pub fn clear_changes(&mut self) {
        self.journal.clear();
        self.memory_journal.clear();
        self.triggers.clear();
    }

    /// Records a named event: `event done;`.
    ///
    /// It goes in a namespace of its own rather than into the signal map,
    /// because an event has no value to hold and reading one has to be an
    /// error rather than a number.
    pub fn declare_event(&mut self, name: impl Into<String>) {
        self.events.insert(name.into());
    }

    /// Whether `name` was declared as an event.
    pub fn is_event(&self, name: &str) -> bool {
        !self.events.is_empty() && self.events.contains(name)
    }

    /// Whether the design declares any event at all. `false` is exact, and it
    /// is what keeps the settle loop from asking anything else of a design
    /// that has none.
    pub fn any_event(&self) -> bool {
        !self.events.is_empty()
    }

    /// Fires a named event, reporting whether the name was one.
    ///
    /// Nothing is stored but the fact that it happened: the settle loop takes
    /// the trigger, turns it into an edge, and the event is over. Triggering
    /// the same event twice before a round takes them wakes a block once,
    /// which is what two triggers inside one time step mean.
    pub fn trigger_event(&mut self, name: &str) -> bool {
        if !self.is_event(name) {
            return false;
        }
        if !self.triggers.iter().any(|fired| fired == name) {
            self.triggers.push(name.to_string());
        }
        true
    }

    /// Holds an already-evaluated right hand side until the timing control in
    /// front of it expires.
    pub fn hold(&mut self, slot: String, value: Register) {
        self.holds.insert(slot, value);
    }

    /// Takes back what [`hold`](StateStore::hold) put there. A slot is written
    /// once and read once, so it is removed rather than left behind.
    pub fn take_hold(&mut self, slot: &str) -> Option<Register> {
        self.holds.remove(slot)
    }

    /// Every event triggered since the last call, clearing the journal so the
    /// next round is measured from here. This is what makes a trigger wake a
    /// block exactly once: the round that takes it is the only round that can
    /// see it.
    pub fn take_triggers(&mut self) -> Vec<String> {
        std::mem::take(&mut self.triggers)
    }

    /// Declares a signal over `(msb, lsb)`, initialized to all `x` the way an
    /// unassigned Verilog `reg` starts out.
    pub fn declare(&mut self, name: impl Into<String>, range: (i64, i64)) {
        self.declare_signed(name, range, false);
    }

    /// [`declare`](StateStore::declare) for a signal whose declaration carried
    /// a `signed` qualifier.
    pub fn declare_signed(&mut self, name: impl Into<String>, range: (i64, i64), signed: bool) {
        self.declare_filled(name, range, signed, false, Register::unknown);
    }

    /// Declares a *net* — a `wire`, `tri` or a port backed by one — which
    /// starts at `z` rather than `x`.
    ///
    /// The difference is not cosmetic: a variable with no assignment holds `x`
    /// because nothing has said what it is, while a net with no driver holds
    /// `z` because nothing is driving it, and `z` is what a reader sees. So an
    /// undriven bit of `out` reads `z` where an untouched `reg` reads `x` —
    /// which is what iverilog prints, and what a three-state bus depends on.
    pub fn declare_net(&mut self, name: impl Into<String>, range: (i64, i64), signed: bool) {
        self.declare_filled(name, range, signed, true, Register::high_impedance);
    }

    /// Turns a signal that was declared a net into a variable, keeping the
    /// width it was declared at and refilling it with `x`.
    ///
    /// This is what an **aliased** port that a child backs with a `reg` asks
    /// for. The parent's `wire w;` and the child's `output reg w` are one store
    /// entry here, so only one of the two fills can stand — and it is the
    /// child's, because the `reg` is a driver: `w` reads `x` because its driver
    /// has not said what it is, not `z` because nothing is driving it.
    pub fn redeclare_as_variable(&mut self, name: &str) {
        let Some(signal) = self.name_to_signal.get(name) else {
            return;
        };
        let range = signal.range();
        let signed = signal.is_signed();
        self.name_to_signal.insert(
            name.to_string(),
            SignalState::with_range(Register::unknown(range_width(range)), range)
                .with_signedness(signed),
        );
    }

    /// Declares a `real`: sixty-four bits read as a double, starting at `0.0`.
    ///
    /// It is the one variable that does **not** start unknown, and that is a
    /// property of the type rather than a choice — a double has no `x` to hold.
    /// An unwritten `real` reads `0.000000`, which is what iverilog prints.
    pub fn declare_real(&mut self, name: impl Into<String>) {
        let name = name.into();
        self.record(&name);
        self.any_signed = true;
        self.any_real = true;
        self.name_to_signal.insert(
            name,
            SignalState::with_range(Register::from_f64(0.0), REAL_RANGE),
        );
    }

    /// Declares an array of `real`s: `real samples [0:3];`.
    pub fn declare_real_memory(&mut self, name: impl Into<String>, addresses: Vec<(i64, i64)>) {
        self.any_signed = true;
        self.any_real = true;
        self.insert_memory(name, Memory::of_reals(addresses), true);
    }

    fn declare_filled(
        &mut self,
        name: impl Into<String>,
        range: (i64, i64),
        signed: bool,
        net: bool,
        fill: fn(usize) -> Register,
    ) {
        let name = name.into();
        self.record(&name);
        self.any_signed |= signed;
        let register = fill(range_width(range));
        self.name_to_signal.insert(
            name,
            SignalState::with_range(register, range)
                .with_signedness(signed)
                .as_net(net),
        );
    }

    /// Declares a memory of `addresses` words, each over `range`, every bit
    /// `x` the way an unassigned Verilog `reg` starts out.
    ///
    /// The name goes into the memory map instead of the signal map, and that is
    /// the only record anything downstream has of the declaration having had an
    /// address dimension — it is what makes `mem[3]` a word select and `a[3]` a
    /// bit select.
    pub fn declare_memory(
        &mut self,
        name: impl Into<String>,
        addresses: Vec<(i64, i64)>,
        range: (i64, i64),
        signed: bool,
    ) {
        self.insert_memory(name, Memory::new(addresses, range, signed), signed);
    }

    /// [`declare_memory`](StateStore::declare_memory) for an array of *nets*,
    /// whose undriven words read `z`.
    pub fn declare_net_memory(
        &mut self,
        name: impl Into<String>,
        addresses: Vec<(i64, i64)>,
        range: (i64, i64),
        signed: bool,
    ) {
        self.insert_memory(name, Memory::of_nets(addresses, range, signed), signed);
    }

    fn insert_memory(&mut self, name: impl Into<String>, memory: Memory, signed: bool) {
        self.any_signed |= signed;
        self.any_memory = true;
        self.name_to_memory.insert(name.into(), memory);
    }

    /// Puts a memory that already exists into this store under `name`.
    ///
    /// The one caller is [`FunctionDefinition::call`](crate::simulator::program::FunctionDefinition::call),
    /// which seeds a frame with copies of what the body reads: a memory has no
    /// declaration to repeat here, only words that were filled somewhere else.
    pub fn adopt_memory(&mut self, name: impl Into<String>, memory: Memory) {
        let signed = memory.is_signed();
        self.insert_memory(name, memory, signed);
    }

    /// Everything declared under `prefix` — its signals, its memories and its
    /// events — as it stands now, keyed by what follows the prefix.
    ///
    /// This is the prototype of a `task automatic`'s storage: taken once,
    /// before the design has run, and laid down again under a fresh prefix by
    /// [`install_scope`](StateStore::install_scope) for every activation, so
    /// each one starts with the declarations — widths, signedness, realness,
    /// memory shapes — and the untouched values of its own.
    pub fn scope_storage(&self, prefix: &str) -> ScopeStorage {
        let inside = |name: &String| name.strip_prefix(prefix).map(str::to_string);
        ScopeStorage {
            signals: self
                .name_to_signal
                .iter()
                .filter_map(|(name, signal)| Some((inside(name)?, signal.clone())))
                .collect(),
            memories: self
                .name_to_memory
                .iter()
                .filter_map(|(name, memory)| Some((inside(name)?, memory.clone())))
                .collect(),
            events: self.events.iter().filter_map(inside).collect(),
        }
    }

    /// Lays `storage` down under `prefix`, replacing whatever an earlier
    /// activation left there.
    ///
    /// Nothing is journalled: the names are fresh storage rather than values
    /// that moved, and the only thing that reads them is the activation about
    /// to start.
    pub fn install_scope(&mut self, storage: &ScopeStorage, prefix: &str) {
        for (name, signal) in &storage.signals {
            self.name_to_signal
                .insert(format!("{}{}", prefix, name), signal.clone());
        }
        for (name, memory) in &storage.memories {
            self.name_to_memory
                .insert(format!("{}{}", prefix, name), memory.clone());
        }
        for name in &storage.events {
            self.events.insert(format!("{}{}", prefix, name));
        }
    }

    /// Whether the design declares any memory. `false` is exact.
    pub fn any_memory(&self) -> bool {
        self.any_memory
    }

    /// The memory `name` declares, if it is a memory rather than a signal.
    pub fn memory(&self, name: &str) -> Option<&Memory> {
        self.name_to_memory.get(name)
    }

    /// Writes one word of a memory, reporting whether the stored value moved —
    /// or `None` when `name` is not a memory at all.
    ///
    /// The write is journalled **per word**, because a block may be sensitive
    /// to one word and not to its neighbours: `always @(dummy[m])` in a
    /// generate loop is one block per index, and a write to `dummy[0]` must
    /// wake only the first (corpus `pr2815398a_std`). A repeat write of the
    /// word written last is folded in here, which keeps a loop hammering one
    /// word from growing the journal; everything else is merged when it is
    /// taken. A word is keyed by its flat position in the memory rather than
    /// by the indices it was written with, which names one word in any number
    /// of dimensions and costs a journalled write no allocation.
    pub fn set_word(&mut self, name: &str, address: &[i64], value: &Register) -> Option<bool> {
        let memory = self.name_to_memory.get_mut(name)?;
        let before = memory.word(Some(address));
        if !memory.set_word(address, value) {
            return Some(false);
        }
        let after = memory.word(Some(address));
        // A write that moved a word named a word, so it has a position.
        let position = memory.word_position(address)?;
        match self.memory_journal.last_mut() {
            Some(last) if last.position == position && last.name == name => last.after = after,
            _ => self.memory_journal.push(MemoryChange {
                name: name.to_string(),
                position,
                before,
                after,
            }),
        }
        Some(true)
    }

    /// Writes bits of one word of a memory — `mem[addr][3:1] = d;` — reporting
    /// whether the stored value moved, or `None` when `name` is not a memory.
    ///
    /// The bits are placed through the memory's declared range and the word
    /// then goes back through [`StateStore::set_word`], so the change is
    /// journalled exactly as a whole-word write is. `indices` runs most
    /// significant first, the order `ResolvedTarget::Bits` keeps.
    pub fn set_word_bits(
        &mut self,
        name: &str,
        address: &[i64],
        indices: &[i64],
        value: &Register,
    ) -> Option<bool> {
        let memory = self.name_to_memory.get(name)?;
        let mut word = memory.word(Some(address));
        let value = value.coerced(indices.len());
        for (offset, &index) in indices.iter().enumerate() {
            word = memory.with_bit_of(word, index, value.get_raw()[offset]);
        }
        self.set_word(name, address, &word)
    }

    /// Every memory word written since the last call, one entry per word —
    /// the value the round first displaced and the last one written — clearing
    /// the journal so the next round is measured from here.
    ///
    /// A word that was written and then put back is no change, and is dropped
    /// here for the reason `edges_from_changes` drops a signal that did the
    /// same.
    pub fn take_memory_changes(&mut self) -> Vec<MemoryChange> {
        let mut changes = std::mem::take(&mut self.memory_journal);
        if changes.len() < 2 {
            self.round_words = changes.clone();
            return changes;
        }
        // Stable, so the entries for one word stay in the order they were
        // written and the first holds what the round displaced.
        changes.sort_by(|left, right| {
            (left.name.as_str(), left.position).cmp(&(right.name.as_str(), right.position))
        });
        let mut merged: Vec<MemoryChange> = Vec::with_capacity(changes.len());
        for change in changes {
            match merged.last_mut() {
                Some(last) if last.position == change.position && last.name == change.name => {
                    last.after = change.after;
                }
                _ => merged.push(change),
            }
        }
        merged.retain(|change| change.before != change.after);
        self.round_words = merged.clone();
        merged
    }

    /// What the word of memory `name` at `address` did in the settle round now
    /// running, or `None` when the round did not move it — or when `address`
    /// names no word, which an index outside a dimension, or the wrong number
    /// of indices, does. Taking the memory journal is what starts a round, so
    /// this answers for exactly the edges
    /// [`take_memory_changes`](StateStore::take_memory_changes) handed out.
    pub fn round_word(&self, name: &str, address: &[i64]) -> Option<&MemoryChange> {
        let position = self.name_to_memory.get(name)?.word_position(address)?;
        self.round_words
            .iter()
            .find(|change| change.position == position && change.name == name)
    }

    /// How a write to `name` has to be *read* — signed or not, real or not —
    /// which is the declaration's business rather than the value's, since a
    /// value cannot change a declaration. A name that does not exist yet is
    /// being declared by this very write, so it takes the value's own reading
    /// instead. Both flags come out of one lookup because a write asks for
    /// both and hashing the name twice would be the cost of asking.
    fn declared_reading(&self, name: &str, register: &Register) -> (bool, bool) {
        match self.name_to_signal.get(name) {
            Some(signal) => (signal.is_signed(), signal.is_real()),
            None => (register.is_signed(), register.is_real()),
        }
    }

    /// `register` as the declaration of `name` says it is to be read.
    ///
    /// A real declaration *converts* rather than re-stamps: the bits of a
    /// double are not the bits of the integer that denotes the same number, so
    /// a write of `3` into a `real` has to become `3.0` and a write of `2.5`
    /// into an `integer` has to become `3`. Everything else only re-stamps.
    fn as_declared(&self, name: &str, register: Register) -> (Register, bool) {
        let (signed, real) = self.declared_reading(name, &register);
        if real != register.is_real() {
            let converted = if real {
                Register::from_f64(register.to_f64())
            } else {
                Register::integer_from_f64(register.to_f64().round(), register.width())
            };
            return (converted.with_signedness(signed), signed);
        }
        (register.with_signedness(signed).with_realness(real), signed)
    }

    /// Sets a signal's value. A previously declared range is preserved when the
    /// widths still agree; otherwise the signal is (re)declared as `(width - 1, 0)`.
    pub fn set(&mut self, name: impl Into<String>, register: Register) {
        let name = name.into();
        self.record(&name);
        let (register, signed) = self.as_declared(&name, register);
        self.any_signed |= signed;
        self.any_real |= register.is_real();
        let declared = self.name_to_signal.get(&name);
        let net = declared.is_some_and(SignalState::is_net);
        let range = declared
            .map(|signal| signal.range())
            .filter(|&range| range_width(range) == register.width());
        let mut signal = match range {
            Some(range) => SignalState::with_range(register, range),
            None => SignalState::new(register),
        };
        // Who drives the signal survives a write to it — see `set_ranged`.
        if let Some(declared) = self.name_to_signal.get_mut(&name) {
            signal.strengths = declared.strengths.take();
            signal.drivers = declared.drivers.take();
        }
        self.name_to_signal.insert(name, signal.as_net(net));
    }

    /// Sets a signal's value and declared range in one step.
    pub fn set_ranged(&mut self, name: impl Into<String>, register: Register, range: (i64, i64)) {
        let name = name.into();
        self.record(&name);
        let (register, signed) = self.as_declared(&name, register);
        self.any_signed |= signed;
        self.any_real |= register.is_real();
        // Every whole-signal write in the simulator lands here, so the entry
        // is overwritten in place: one lookup finds it, keeps its declared net
        // flag and replaces the rest, where asking for the flag and then
        // inserting would hash the name twice.
        match self.name_to_signal.get_mut(&name) {
            Some(signal) => {
                let net = signal.is_net();
                // The strength each bit was resolved at and the drivers that
                // reached it describe *who drives the signal*, which a write
                // does not change — so they survive one, the way the declared
                // net flag does. A write that moves a bit the recorded
                // strength no longer describes is what the value check in
                // `tasks::strengths` is for.
                let strengths = signal.strengths.take();
                let drivers = signal.drivers.take();
                *signal = SignalState::with_range(register, range).as_net(net);
                signal.strengths = strengths;
                signal.drivers = drivers;
            }
            None => {
                self.name_to_signal
                    .insert(name, SignalState::with_range(register, range));
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<&Register> {
        self.name_to_signal.get(name).map(|s| s.register())
    }

    pub fn get_signal(&self, name: &str) -> Option<&SignalState> {
        self.name_to_signal.get(name)
    }

    /// The strength every bit of a signal was last resolved at, `width` of
    /// them, most significant first.
    ///
    /// A signal nothing has resolved yet answers from its *value*, which is
    /// what an ordinary driver would have given it anyway: a `z` bit is driven
    /// by nothing and every other bit is `strong`. That is what makes the
    /// first pass over a net agree with every pass after it.
    pub fn strengths_of(&self, name: &str, width: usize) -> Vec<Strength> {
        match self.name_to_signal.get(name) {
            Some(signal) => match signal.strengths() {
                Some(levels) if levels.len() == width => levels.to_vec(),
                _ => signal
                    .register()
                    .get_raw()
                    .iter()
                    .take(width)
                    .map(|code| Strength::driven(*code, DriveStrength::STRONG))
                    .chain(std::iter::repeat(Strength::HIGHZ))
                    .take(width)
                    .collect(),
            },
            None => vec![Strength::HIGHZ; width],
        }
    }

    /// Records what each bit of a net resolved to. Only
    /// `Simulator::resolve_contributions` calls this, which is why every other
    /// signal's slot stays `None`.
    pub fn set_strengths(&mut self, name: &str, levels: Vec<Strength>) {
        if let Some(signal) = self.name_to_signal.get_mut(name) {
            signal.strengths = Some(levels);
        }
    }

    /// Records how many drivers reached each bit of a net. Only
    /// `Simulator::resolve_contributions` calls this, and only for a design
    /// that calls `$countdrivers`.
    pub fn set_driver_counts(&mut self, name: &str, counts: Vec<DriverTally>) {
        if let Some(signal) = self.name_to_signal.get_mut(name) {
            signal.drivers = Some(counts);
        }
    }

    /// Whether the design asks `$countdrivers` anywhere, which is what decides
    /// if the driver tally is kept at all. A design that does not pay one
    /// `bool` per propagation pass.
    pub fn counts_drivers(&self) -> bool {
        self.counts_drivers
    }

    pub fn count_drivers(&mut self) {
        self.counts_drivers = true;
    }

    /// The store entry a port aliased onto its parent's signal really is.
    ///
    /// A testbench reaching into an instance writes the port's own name
    /// (`pad1.pad`), and flattening left no entry under it — the port and what
    /// it was bound to are one entry under the *parent's* name. Only a name
    /// the store does not have is looked up, so an ordinary one costs the hash
    /// it already cost.
    pub fn unalias<'a>(&'a self, name: &'a str) -> &'a str {
        if self.name_to_signal.contains_key(name) {
            return name;
        }
        match self.aliases.get(name) {
            Some(entry) => entry.as_str(),
            None => name,
        }
    }

    pub fn name_aliases(&mut self, aliases: Rc<HashMap<String, String>>) {
        self.aliases = aliases;
    }

    /// A signal for in-place modification. What it holds now is journalled
    /// first, since the caller is free to move it.
    pub fn get_signal_mut(&mut self, name: &str) -> Option<&mut SignalState> {
        self.record(name);
        self.name_to_signal.get_mut(name)
    }

    pub fn contains(&self, name: &str) -> bool {
        self.name_to_signal.contains_key(name)
    }

    pub fn len(&self) -> usize {
        self.name_to_signal.len()
    }

    pub fn is_empty(&self) -> bool {
        self.name_to_signal.is_empty()
    }

    /// Every signal name, sorted.
    pub fn names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.name_to_signal.keys().map(|k| k.as_str()).collect();
        names.sort();
        names
    }
}

/// One call's place on the stack, which it gives back when it is dropped.
pub struct CallGuard<'a>(&'a StateStore);

impl Drop for CallGuard<'_> {
    fn drop(&mut self) {
        self.0.call_depth.set(self.0.call_depth.get() - 1);
    }
}

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for name in self.names() {
            writeln!(f, "{}: {}", name, self.name_to_signal[name].register())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_store_display_is_sorted_by_name() {
        let mut state_store = StateStore::new();
        state_store.set("reg2", Register::from_binary("010"));
        state_store.set("reg1", Register::from_binary("101"));

        assert_eq!(format!("{}", state_store), "reg1: 101\nreg2: 010\n");
    }

    #[test]
    fn test_set_and_get() {
        let mut state_store = StateStore::new();
        state_store.set("reg1", Register::from_binary("111"));

        assert!(state_store.contains("reg1"));
        assert_eq!(state_store.get("reg1"), Some(&Register::from_binary("111")));
        assert_eq!(state_store.get("nope"), None);
        assert_eq!(state_store.len(), 1);
        assert!(!state_store.is_empty());
    }

    #[test]
    fn test_set_multiple() {
        let mut state_store = StateStore::new();
        state_store.set("reg1", Register::from_binary("101"));
        state_store.set("reg2", Register::from_binary("010"));

        assert_eq!(state_store.names(), vec!["reg1", "reg2"]);
        assert_eq!(state_store.get("reg1").unwrap().to_binary(), "101");
        assert_eq!(state_store.get("reg2").unwrap().to_binary(), "010");
    }

    #[test]
    fn test_set_overwrite() {
        let mut state_store = StateStore::new();
        state_store.set("reg1", Register::from_binary("101"));
        state_store.set("reg1", Register::from_binary("000"));

        assert_eq!(state_store.len(), 1);
        assert_eq!(state_store.get("reg1").unwrap().to_binary(), "000");
    }

    #[test]
    fn test_declare_starts_unknown() {
        let mut state_store = StateStore::new();
        state_store.declare("bus", (7, 0));

        assert_eq!(state_store.get("bus").unwrap().to_binary(), "xxxxxxxx");
        assert_eq!(state_store.get_signal("bus").unwrap().range(), (7, 0));
    }

    #[test]
    fn test_set_preserves_declared_range() {
        let mut state_store = StateStore::new();
        state_store.declare("bus", (11, 4));
        state_store.set("bus", Register::from_binary("00001111"));

        assert_eq!(state_store.get_signal("bus").unwrap().range(), (11, 4));
    }

    #[test]
    fn test_set_redeclares_on_width_change() {
        let mut state_store = StateStore::new();
        state_store.declare("bus", (11, 4));
        state_store.set("bus", Register::from_binary("101"));

        assert_eq!(state_store.get_signal("bus").unwrap().range(), (2, 0));
    }

    #[test]
    fn test_signal_bit_positions_descending() {
        let signal = SignalState::with_range(Register::from_binary("1000"), (7, 4));

        assert_eq!(signal.bit_position(7), Some(0));
        assert_eq!(signal.bit_position(4), Some(3));
        assert_eq!(signal.bit_position(3), None);
        assert_eq!(signal.bit_position(8), None);
        assert_eq!(signal.bit(7), 1);
        assert_eq!(signal.bit(6), 0);
        // Out of range reads are x.
        assert_eq!(signal.bit(9), X);
    }

    #[test]
    fn test_signal_bit_positions_ascending() {
        let signal = SignalState::with_range(Register::from_binary("1000"), (0, 3));

        assert_eq!(signal.bit_position(0), Some(0));
        assert_eq!(signal.bit_position(3), Some(3));
        assert_eq!(signal.bit_position(4), None);
        assert_eq!(signal.bit(0), 1);
        assert_eq!(signal.bit(1), 0);
    }

    #[test]
    fn test_signal_set_bit() {
        let mut signal = SignalState::with_range(Register::unknown(4), (7, 4));

        assert!(signal.set_bit(7, 1));
        assert!(signal.set_bit(4, 0));
        assert_eq!(signal.register().to_binary(), "1xx0");
        // Rewriting the same value is not a change.
        assert!(!signal.set_bit(7, 1));
        // Out of range writes are discarded.
        assert!(!signal.set_bit(3, 1));
        assert_eq!(signal.register().to_binary(), "1xx0");
    }

    #[test]
    fn test_memory_holds_one_register_per_declared_address() {
        let mut store = StateStore::new();
        store.declare_memory("mem", vec![(0, 255)], (7, 0), false);

        let memory = store.memory("mem").expect("mem should be a memory");
        assert_eq!(memory.depth(), 256);
        assert_eq!(memory.width(), 8);
        assert_eq!(memory.addresses(), [(0, 255)]);
        assert_eq!(memory.range(), (7, 0));
        // A name is a signal or a memory, never both — which is exactly what
        // tells a word select from a bit select.
        assert!(store.get_signal("mem").is_none());
        assert!(store.any_memory());
    }

    #[test]
    fn test_memory_words_start_unknown_and_are_independent() {
        let mut store = StateStore::new();
        store.declare_memory("mem", vec![(0, 3)], (7, 0), false);

        assert_eq!(
            store.memory("mem").unwrap().word(Some(&[0])).to_binary(),
            "xxxxxxxx"
        );

        store.set_word("mem", &[1], &Register::from_binary("00000001"));
        store.set_word("mem", &[2], &Register::from_binary("00000010"));

        let memory = store.memory("mem").unwrap();
        assert_eq!(memory.word(Some(&[0])).to_binary(), "xxxxxxxx");
        assert_eq!(memory.word(Some(&[1])).to_binary(), "00000001");
        assert_eq!(memory.word(Some(&[2])).to_binary(), "00000010");
        assert_eq!(memory.word(Some(&[3])).to_binary(), "xxxxxxxx");
    }

    #[test]
    fn test_memory_addresses_run_ascending_or_descending() {
        let ascending = Memory::new(vec![(0, 3)], (7, 0), false);
        assert_eq!(ascending.word_position(&[0]), Some(0));
        assert_eq!(ascending.word_position(&[3]), Some(3));
        assert_eq!(ascending.word_position(&[4]), None);
        assert_eq!(ascending.word_position(&[-1]), None);

        // `reg [7:0] m [15:8];` addresses 15 down to 8, and nothing else.
        let descending = Memory::new(vec![(15, 8)], (7, 0), false);
        assert_eq!(descending.word_position(&[15]), Some(0));
        assert_eq!(descending.word_position(&[8]), Some(7));
        assert_eq!(descending.word_position(&[7]), None);
        assert_eq!(descending.word_position(&[16]), None);
    }

    #[test]
    fn test_memory_out_of_range_reads_x_and_discards_a_write() {
        let mut store = StateStore::new();
        store.declare_memory("mem", vec![(0, 3)], (3, 0), false);

        assert_eq!(
            store.set_word("mem", &[9], &Register::from_binary("1111")),
            Some(false)
        );
        let memory = store.memory("mem").unwrap();
        assert_eq!(memory.word(Some(&[9])).to_binary(), "xxxx");
        // An index that did not evaluate to a number reads `x` as well.
        assert_eq!(memory.word(None).to_binary(), "xxxx");
        assert!(memory.words.iter().all(|word| word.to_binary() == "xxxx"));
    }

    #[test]
    fn test_memory_write_is_resized_and_keeps_the_declared_signedness() {
        let mut store = StateStore::new();
        store.declare_memory("mem", vec![(0, 1)], (31, 0), true);

        store.set_word("mem", &[0], &Register::from_binary("1010"));
        let word = store.memory("mem").unwrap().word(Some(&[0]));
        assert_eq!(word.width(), 32);
        assert!(word.is_signed(), "a value may not change a declaration");
    }

    #[test]
    fn test_memory_writes_are_journalled_so_a_block_can_wake_on_them() {
        let mut store = StateStore::new();
        store.declare_memory("mem", vec![(0, 3)], (3, 0), false);
        store.clear_changes();

        // Rewriting the same value is not a change and is not journalled.
        store.set_word("mem", &[0], &Register::from_binary("0001"));
        store.set_word("mem", &[0], &Register::from_binary("0001"));
        store.set_word("mem", &[1], &Register::from_binary("0010"));

        // One entry per *word*: each pair is what the round displaced from that
        // word and what it last wrote there.
        let changes = store.take_memory_changes();
        assert_eq!(changes.len(), 2);
        assert_eq!((changes[0].name.as_str(), changes[0].position), ("mem", 0));
        assert_eq!(changes[0].before.to_binary(), "xxxx");
        assert_eq!(changes[0].after.to_binary(), "0001");
        assert_eq!((changes[1].name.as_str(), changes[1].position), ("mem", 1));
        assert_eq!(changes[1].after.to_binary(), "0010");
        assert!(store.take_memory_changes().is_empty());
    }

    /// A word of a two-dimensional array is one journal entry keyed by its
    /// flat position, and [`StateStore::round_word`] finds it by the full
    /// address — a neighbour in either dimension is a different word, and an
    /// address with too few indices names no word at all.
    #[test]
    fn test_a_two_dimensional_memory_word_is_journalled_by_its_whole_address() {
        let mut store = StateStore::new();
        store.declare_memory("grid", vec![(0, 1), (0, 2)], (3, 0), false);
        store.clear_changes();

        store.set_word("grid", &[1, 2], &Register::from_binary("0101"));
        let changes = store.take_memory_changes();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].position, 5);

        assert!(store.round_word("grid", &[1, 2]).is_some());
        assert!(store.round_word("grid", &[1, 1]).is_none());
        assert!(store.round_word("grid", &[0, 2]).is_none());
        assert!(store.round_word("grid", &[1]).is_none());
    }

    /// Writing a word and then putting its old value back within one round
    /// is no change, and a word written twice around another keeps what the
    /// round first displaced.
    #[test]
    fn test_a_memory_word_journal_merges_and_drops_round_trips() {
        let mut store = StateStore::new();
        store.declare_memory("mem", vec![(0, 3)], (3, 0), false);
        store.set_word("mem", &[0], &Register::from_binary("0001"));
        store.set_word("mem", &[1], &Register::from_binary("0001"));
        store.clear_changes();

        store.set_word("mem", &[0], &Register::from_binary("0010"));
        store.set_word("mem", &[1], &Register::from_binary("0100"));
        store.set_word("mem", &[0], &Register::from_binary("0001"));
        store.set_word("mem", &[1], &Register::from_binary("1000"));

        let changes = store.take_memory_changes();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].position, 1);
        assert_eq!(changes[0].before.to_binary(), "0001");
        assert_eq!(changes[0].after.to_binary(), "1000");
    }

    #[test]
    fn test_writing_a_word_of_something_that_is_not_a_memory_is_reported() {
        let mut store = StateStore::new();
        store.declare("plain", (7, 0));

        assert_eq!(
            store.set_word("plain", &[0], &Register::from_binary("1")),
            None
        );
        assert_eq!(
            store.set_word("absent", &[0], &Register::from_binary("1")),
            None
        );
    }

    /// The standard generator, checked against the first four seed/value pairs
    /// of corpus `pr995`'s gold file — which is iverilog 12.0's own output for
    /// `result = $random(seed);` starting from a seed of `1`.
    #[test]
    fn test_random_from_seed_matches_the_standard() {
        let mut seed = 1i32;
        let mut pairs = Vec::new();
        for _ in 0..4 {
            let (value, next) = random_from_seed(seed);
            seed = next;
            pairs.push((next as u32, value as u32));
        }
        assert_eq!(
            pairs,
            vec![
                (0x0001_0dce, 0x8001_0e00),
                (0x1c59_83f7, 0x9c59_8438),
                (0xc359_37cc, 0x4359_3986),
                (0x2e13_0a5d, 0xae13_0c5c),
            ]
        );
    }

    /// A seed of zero is a stream of its own rather than a degenerate one: the
    /// generator replaces it, so the unseeded default draws real numbers.
    /// Measured against iverilog 12.0's first unseeded `$random`, 303379748.
    #[test]
    fn test_random_default_seed_draws_a_real_number() {
        let store = StateStore::new();
        assert_eq!(store.next_random(), 303379748);
    }

    #[test]
    fn test_signal_new_uses_implicit_range() {
        let signal = SignalState::new(Register::from_binary("1010"));

        assert_eq!(signal.range(), (3, 0));
        assert_eq!(signal.width(), 4);
        assert_eq!(signal.bit(0), 0);
        assert_eq!(signal.bit(3), 1);
    }
}
