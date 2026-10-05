//! `visilog serve` — a design in a browser, stepped through time.
//!
//! The viewer is a single page served from the binary itself: the design's
//! hierarchy drawn as nested boxes, every port and register showing its value,
//! a waveform strip for the signals pinned to it, and controls that step the
//! simulation forward. It is a *reference client* of the two contracts the
//! library already offers — [`graph::design_graph`] for structure and
//! [`inspect::Session`] for execution — and adds nothing to either: whatever
//! it can show, an embedding client can show the same way.
//!
//! The server is deliberately small. It speaks just enough HTTP/1.1 over
//! [`std::net`] to answer one browser, one request at a time, which is what a
//! [`Session`] — single-threaded by construction — wants anyway, and it takes
//! no dependency the WebAssembly build of the library would have to carry.
//!
//! **Going back in time is history, not re-simulation.** The simulator only
//! runs forwards, so the server subscribes to every signal and keeps every
//! change a run produces; a value at an earlier time is read out of that
//! record. The record is bounded ([`HISTORY_LIMIT`]): past it, the server
//! stops recording, says so, and the waveforms end where the record does.

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};

use serde::Deserialize;
use serde_json::{json, Value as Json};

use crate::graph::{design_graph, DesignGraph};
use crate::inspect::{Condition, Limits, Session, Value};
use crate::run::{load_design, RunConfig, StopReason};

/// How many value changes the server keeps for scrubbing back and for the
/// waveforms, across every signal. A few hundred megabytes at worst.
pub const HISTORY_LIMIT: usize = 4_000_000;

/// How many timesteps one request may run before it stops and hands control
/// back, so a "run to the next edge" of a signal that never moves cannot hang
/// the page.
const STEPS_PER_REQUEST: u64 = 2_000_000;

const INDEX: &str = include_str!("viewer/index.html");
const SCRIPT: &str = include_str!("viewer/viewer.js");
const STYLE: &str = include_str!("viewer/viewer.css");

/// One design, being stepped, with everything it has done so far.
pub struct Viewer {
    config: RunConfig,
    session: Session,
    graph: DesignGraph,
    /// Every signal with a value, in the session's order; `history` and
    /// `index` follow it.
    ids: Vec<String>,
    index: HashMap<String, usize>,
    /// Per signal, `(time, value)` for every change, the first entry being the
    /// value before the run began.
    history: Vec<Vec<(i64, Value)>>,
    recorded: usize,
    truncated: bool,
}

/// What `/api/run` is asked to do.
#[derive(Debug, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum RunRequest {
    /// One timestep.
    Step,
    /// `count` timesteps.
    Steps { count: u64 },
    /// Every timestep up to and including `time` (in clock ticks).
    Until { time: i64 },
    /// Until `signal` next rises (`posedge`), falls (`negedge`) or moves at
    /// all (`any`).
    Edge { signal: String, edge: String },
    /// Until the Verilog expression `expression` becomes true.
    Breakpoint { expression: String },
    /// Until the design finishes or has nothing left to do.
    Run,
}

impl Viewer {
    pub fn new(config: RunConfig) -> Result<Viewer, String> {
        let design = load_design(&config).map_err(|error| error.to_string())?;
        let graph =
            design_graph(&design.modules, &design.top).map_err(|error| error.to_string())?;
        let mut session = Session::new(&config).map_err(|error| error.to_string())?;
        let ids: Vec<String> = session
            .signals()
            .iter()
            .filter(|signal| session.value(&signal.id).is_ok())
            .map(|signal| signal.id.clone())
            .collect();
        session
            .subscribe(ids.iter().map(String::as_str))
            .map_err(|error| format!("{:?}", error))?;
        session.set_change_capacity(usize::MAX);
        let history = ids
            .iter()
            .map(|id| {
                vec![(
                    i64::MIN,
                    session.value(id).expect("listed because it has a value"),
                )]
            })
            .collect();
        let index = ids
            .iter()
            .enumerate()
            .map(|(position, id)| (id.clone(), position))
            .collect();
        Ok(Viewer {
            config,
            session,
            graph,
            ids,
            index,
            history,
            recorded: 0,
            truncated: false,
        })
    }

    /// Moves what the last run changed into the history.
    fn record(&mut self) {
        let batch = self.session.take_changes();
        for change in batch.changes {
            if self.recorded >= HISTORY_LIMIT {
                self.truncated = true;
                break;
            }
            if let Some(&position) = self.index.get(&change.id) {
                let trace = &mut self.history[position];
                // A change at the time already recorded replaces it: the
                // record is one value per signal per timestep.
                if trace.last().is_some_and(|(time, _)| *time == change.time) {
                    trace.pop();
                }
                trace.push((change.time, change.value));
                self.recorded += 1;
            }
        }
        if batch.dropped > 0 {
            self.truncated = true;
        }
    }

    /// The value signal `position` held at `time`.
    fn value_at(&self, position: usize, time: i64) -> &Value {
        let trace = &self.history[position];
        let after = trace.partition_point(|(at, _)| *at <= time);
        &trace[after.saturating_sub(1)].1
    }

    fn design(&self) -> Json {
        let simulator = self.session.simulator();
        json!({
            "graph": self.graph,
            "signals": self.session.signals(),
            "top": self.session.top(),
            "tick_femtoseconds": simulator.tick_femtoseconds(),
            "ticks_per_unit": simulator.ticks_per_unit(),
            "sources": self.config.sources.iter().map(|path| path.display().to_string()).collect::<Vec<_>>(),
            "state": self.state(),
        })
    }

    fn state(&self) -> Json {
        json!({
            "time": self.session.now(),
            "steps": self.session.steps(),
            "finished": self.session.finished(),
            "console_length": self.session.simulator().output().text().len(),
            "truncated": self.truncated,
        })
    }

    /// Every signal's value at `time`, or now.
    fn values(&self, time: Option<i64>) -> Json {
        let time = time.unwrap_or_else(|| self.session.now());
        let values: serde_json::Map<String, Json> = self
            .ids
            .iter()
            .enumerate()
            .map(|(position, id)| (id.clone(), value_json(self.value_at(position, time))))
            .collect();
        json!({ "time": time, "values": values })
    }

    /// The recorded changes of `ids`, for the waveform strip.
    fn traces(&self, ids: &[&str]) -> Json {
        let traces: serde_json::Map<String, Json> = ids
            .iter()
            .filter_map(|id| {
                let position = *self.index.get(*id)?;
                let changes: Vec<Json> = self.history[position]
                    .iter()
                    .map(|(time, value)| json!([time.max(&0), value_json(value)]))
                    .collect();
                Some((id.to_string(), Json::Array(changes)))
            })
            .collect();
        json!({ "now": self.session.now(), "traces": traces })
    }

    fn run(&mut self, request: RunRequest) -> Json {
        let limits = |until: Option<i64>, steps: u64| Limits {
            until,
            steps: Some(steps),
        };
        let mut temporary = None;
        let stop = match request {
            RunRequest::Step => self.session.step_timestep(),
            RunRequest::Steps { count } => self
                .session
                .run(limits(None, count.min(STEPS_PER_REQUEST)), None),
            RunRequest::Until { time } => self
                .session
                .run(limits(Some(time), STEPS_PER_REQUEST), None),
            RunRequest::Run => self.session.run(limits(None, STEPS_PER_REQUEST), None),
            RunRequest::Edge { signal, edge } => {
                let width = self.session.signal(&signal).map_or(1, |info| info.width);
                let condition = match (edge.as_str(), width) {
                    ("posedge", 1) => Condition::Equals(signal, Value::Bits("1".into())),
                    ("negedge", 1) => Condition::Equals(signal, Value::Bits("0".into())),
                    _ => Condition::Changes(signal),
                };
                match self.session.add_breakpoint(condition) {
                    Ok(id) => {
                        temporary = Some(id);
                        self.session.run(limits(None, STEPS_PER_REQUEST), None)
                    }
                    Err(error) => return json!({ "error": format!("{:?}", error) }),
                }
            }
            RunRequest::Breakpoint { expression } => {
                match self
                    .session
                    .add_breakpoint(Condition::Expression(expression))
                {
                    Ok(id) => {
                        temporary = Some(id);
                        self.session.run(limits(None, STEPS_PER_REQUEST), None)
                    }
                    Err(error) => return json!({ "error": format!("{:?}", error) }),
                }
            }
        };
        if let Some(id) = temporary {
            self.session.remove_breakpoint(id);
        }
        self.record();
        json!({
            "stop": {
                "reason": stop_name(stop.reason),
                "error": stop.error,
                "steps": stop.steps,
            },
            "state": self.state(),
        })
    }

    /// Starts the design again from time zero, forgetting the history.
    fn reset(&mut self) -> Result<Json, String> {
        *self = Viewer::new(self.config.clone())?;
        Ok(self.design())
    }

    /// A source file of the design, if `path` is one — and nothing else on
    /// the machine.
    fn source(&self, path: &str) -> Option<String> {
        self.config
            .sources
            .iter()
            .find(|source| source.display().to_string() == path)
            .and_then(|source| std::fs::read_to_string(source).ok())
    }

    fn console(&self, from: usize) -> Json {
        let text = self.session.simulator().output().text();
        let from = from.min(text.len());
        // A byte offset the client sent may fall inside a character.
        let from = (from..=text.len())
            .find(|&at| text.is_char_boundary(at))
            .unwrap_or(text.len());
        json!({ "text": &text[from..], "length": text.len() })
    }
}

fn value_json(value: &Value) -> Json {
    match value {
        Value::Bits(bits) => Json::String(bits.clone()),
        Value::Real(real) => json!(real),
    }
}

fn stop_name(reason: StopReason) -> String {
    serde_json::to_value(reason)
        .ok()
        .and_then(|value| value.as_str().map(str::to_string))
        .unwrap_or_default()
}

/// Serves the viewer for `config` on `port` until the process is stopped.
pub fn serve(config: RunConfig, port: u16) -> std::io::Result<()> {
    let mut viewer = Viewer::new(config).map_err(std::io::Error::other)?;
    let listener = TcpListener::bind(("127.0.0.1", port))?;
    eprintln!(
        "visilog: serving {} at http://127.0.0.1:{}/",
        viewer.session.top(),
        port
    );
    for stream in listener.incoming() {
        let Ok(stream) = stream else { continue };
        if let Err(error) = handle(&mut viewer, stream) {
            eprintln!("visilog: {}", error);
        }
    }
    Ok(())
}

/// One request, start to finish.
fn handle(viewer: &mut Viewer, stream: TcpStream) -> std::io::Result<()> {
    let mut reader = BufReader::new(stream.try_clone()?);
    let mut request_line = String::new();
    reader.read_line(&mut request_line)?;
    let mut parts = request_line.split_whitespace();
    let (method, target) = (parts.next().unwrap_or(""), parts.next().unwrap_or("/"));
    let mut length = 0usize;
    loop {
        let mut header = String::new();
        if reader.read_line(&mut header)? == 0 || header.trim().is_empty() {
            break;
        }
        if let Some((name, value)) = header.split_once(':') {
            if name.eq_ignore_ascii_case("content-length") {
                length = value.trim().parse().unwrap_or(0);
            }
        }
    }
    let mut body = vec![0u8; length.min(1 << 20)];
    reader.read_exact(&mut body)?;

    let (path, query) = target.split_once('?').unwrap_or((target, ""));
    let query: HashMap<String, String> = query
        .split('&')
        .filter_map(|pair| pair.split_once('='))
        .map(|(key, value)| (key.to_string(), percent_decoded(value)))
        .collect();

    let response = match (method, path) {
        ("GET", "/") => Response::text("text/html; charset=utf-8", INDEX),
        ("GET", "/viewer.js") => Response::text("text/javascript; charset=utf-8", SCRIPT),
        ("GET", "/viewer.css") => Response::text("text/css; charset=utf-8", STYLE),
        ("GET", "/api/design") => Response::json(viewer.design()),
        ("GET", "/api/values") => {
            Response::json(viewer.values(query.get("t").and_then(|t| t.parse().ok())))
        }
        ("GET", "/api/traces") => {
            let ids: Vec<&str> = query
                .get("ids")
                .map(|ids| ids.split(',').filter(|id| !id.is_empty()).collect())
                .unwrap_or_default();
            Response::json(viewer.traces(&ids))
        }
        ("GET", "/api/console") => Response::json(
            viewer.console(
                query
                    .get("from")
                    .and_then(|from| from.parse().ok())
                    .unwrap_or(0),
            ),
        ),
        ("GET", "/api/source") => match query.get("path").and_then(|path| viewer.source(path)) {
            Some(text) => Response::text("text/plain; charset=utf-8", &text),
            None => Response::status(404, "not a source of this design"),
        },
        ("POST", "/api/run") => match serde_json::from_slice::<RunRequest>(&body) {
            Ok(request) => Response::json(viewer.run(request)),
            Err(error) => Response::status(400, &error.to_string()),
        },
        ("POST", "/api/reset") => match viewer.reset() {
            Ok(design) => Response::json(design),
            Err(error) => Response::status(500, &error),
        },
        _ => Response::status(404, "not found"),
    };
    response.write_to(stream)
}

struct Response {
    status: u16,
    content_type: &'static str,
    body: Vec<u8>,
}

impl Response {
    fn text(content_type: &'static str, body: &str) -> Response {
        Response {
            status: 200,
            content_type,
            body: body.as_bytes().to_vec(),
        }
    }

    fn json(value: Json) -> Response {
        Response {
            status: 200,
            content_type: "application/json",
            body: serde_json::to_vec(&value).expect("a JSON value serialises"),
        }
    }

    fn status(status: u16, message: &str) -> Response {
        Response {
            status,
            content_type: "text/plain; charset=utf-8",
            body: message.as_bytes().to_vec(),
        }
    }

    fn write_to(self, mut stream: TcpStream) -> std::io::Result<()> {
        let reason = match self.status {
            200 => "OK",
            400 => "Bad Request",
            404 => "Not Found",
            _ => "Internal Server Error",
        };
        write!(
            stream,
            "HTTP/1.1 {} {}\r\nContent-Type: {}\r\nContent-Length: {}\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n",
            self.status,
            reason,
            self.content_type,
            self.body.len()
        )?;
        stream.write_all(&self.body)?;
        stream.flush()
    }
}

/// `%2E` → `.`, `+` → ` ` — what a browser does to a query parameter.
fn percent_decoded(text: &str) -> String {
    let bytes = text.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut index = 0;
    while index < bytes.len() {
        match bytes[index] {
            b'%' if index + 2 < bytes.len() => {
                let hex = std::str::from_utf8(&bytes[index + 1..index + 3]).unwrap_or("");
                match u8::from_str_radix(hex, 16) {
                    Ok(byte) => {
                        out.push(byte);
                        index += 3;
                        continue;
                    }
                    Err(_) => out.push(b'%'),
                }
            }
            b'+' => out.push(b' '),
            byte => out.push(byte),
        }
        index += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn viewer(source: &str) -> Viewer {
        let dir = std::env::temp_dir().join(format!("visilog-serve-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("design.v");
        std::fs::write(&path, source).unwrap();
        Viewer::new(RunConfig {
            sources: vec![path],
            output_dir: Some(dir),
            ..RunConfig::default()
        })
        .unwrap()
    }

    const COUNTER: &str = "
        module counter(input clk, output reg [3:0] count);
            initial count = 0;
            always @(posedge clk) count <= count + 1;
        endmodule
        module tb;
            reg clk = 0;
            wire [3:0] count;
            counter dut(clk, count);
            always #5 clk = ~clk;
        endmodule
    ";

    #[test]
    fn test_stepping_records_history_that_can_be_read_back() {
        let mut viewer = viewer(COUNTER);
        for _ in 0..8 {
            viewer.run(RunRequest::Step);
        }
        let now = viewer.values(None);
        assert_eq!(now["time"], 35);
        assert_eq!(now["values"]["tb.count"], "0100");
        let earlier = viewer.values(Some(12));
        assert_eq!(earlier["values"]["tb.count"], "0001");
        assert_eq!(earlier["values"]["tb.clk"], "0");
        let traces = viewer.traces(&["tb.count"]);
        assert_eq!(traces["traces"]["tb.count"][1], json!([5, "0001"]));
    }

    #[test]
    fn test_run_to_an_edge_stops_on_it() {
        let mut viewer = viewer(COUNTER);
        viewer.run(RunRequest::Step);
        let result = viewer.run(RunRequest::Edge {
            signal: "tb.clk".into(),
            edge: "posedge".into(),
        });
        assert_eq!(result["stop"]["reason"], "breakpoint");
        assert_eq!(result["state"]["time"], 5);
        let result = viewer.run(RunRequest::Breakpoint {
            expression: "tb.count == 3".into(),
        });
        assert_eq!(result["stop"]["reason"], "breakpoint");
        assert_eq!(viewer.values(None)["values"]["tb.count"], "0011");
    }

    #[test]
    fn test_only_the_designs_own_sources_are_served() {
        let viewer = viewer(COUNTER);
        let own = viewer.config.sources[0].display().to_string();
        assert!(viewer
            .source(&own)
            .is_some_and(|text| text.contains("module counter")));
        assert!(viewer.source("/etc/passwd").is_none());
    }

    #[test]
    fn test_a_query_parameter_is_decoded() {
        assert_eq!(percent_decoded("tb.dut%2Ecount+x%2"), "tb.dut.count x%2");
    }
}
