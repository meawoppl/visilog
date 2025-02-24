use crate::parsers::{
    assignment::{ProceduralAssignment, ProceduralAssignmentType},
    behavior::{AlwaysBlock, InitialBlock, ProceduralBlock, ProceduralStatements},
    constants::VerilogConstant,
    delay::Delay,
    expr::Expression,
    identifier::Identifier,
};

#[derive(Debug, PartialEq)]
pub enum SignalErrors {
    SignalEnded,
    SignalInvalid,
}

fn string_decode_signal(input: &str) -> Result<Vec<bool>, SignalErrors> {
    input
        .chars()
        .map(|c| match c {
            '_' => Ok(false),
            '-' => Ok(true),
            _ => Err(SignalErrors::SignalInvalid),
        })
        .collect()
}

pub trait Signal {
    fn evaluate(&self, time: u64) -> Result<bool, SignalErrors>;

    fn embodiment(&self, identifier: Identifier) -> ProceduralBlock;
}

pub struct FiniteSignal {
    values: Vec<bool>,
}

impl FiniteSignal {
    pub fn pattern(pattern: &'static str) -> Self {
        let values = string_decode_signal(pattern).unwrap();
        FiniteSignal { values }
    }
}

impl Signal for FiniteSignal {
    fn evaluate(&self, time: u64) -> Result<bool, SignalErrors> {
        match self.values.get(time as usize) {
            Some(value) => Ok(*value),
            None => Err(SignalErrors::SignalEnded),
        }
    }

    fn embodiment(&self, identifier: Identifier) -> ProceduralBlock {
        let mut statements = vec![];

        for (i, value) in self.values.iter().enumerate() {
            let assignment = ProceduralAssignment::new(
                None,
                Expression::Identifier(identifier.clone()),
                ProceduralAssignmentType::Blocking,
                None,
                Expression::Constant(VerilogConstant::from_int(*value as i64)),
            );

            statements.push(ProceduralStatements::Assignment(assignment));

            if i < self.values.len() - 1 {
                statements.push(ProceduralStatements::Delay(Delay::new(1)));
            }
        }

        ProceduralBlock::InitialBlock(InitialBlock::new(statements))
    }
}

pub struct InfiniteSignal {
    val_loop: Vec<bool>,
}

impl Signal for InfiniteSignal {
    fn evaluate(&self, time: u64) -> Result<bool, SignalErrors> {
        let index = (time as usize) % self.val_loop.len();
        Ok(self.val_loop[index])
    }

    fn embodiment(&self, identifier: Identifier) -> ProceduralBlock {
        let mut statements = vec![];

        for (i, value) in self.val_loop.iter().enumerate() {
            let assignment = ProceduralAssignment::new(
                None,
                Expression::Identifier(identifier.clone()),
                ProceduralAssignmentType::Blocking,
                None,
                Expression::Constant(VerilogConstant::from_int(*value as i64)),
            );

            statements.push(ProceduralStatements::Assignment(assignment));
            statements.push(ProceduralStatements::Delay(Delay::new(1)));
        }

        ProceduralBlock::AlwaysBlock(AlwaysBlock::new(vec![], statements))
    }
}

impl InfiniteSignal {
    pub fn repeating(pattern: &'static str) -> Self {
        let bits = string_decode_signal(pattern).unwrap();
        InfiniteSignal { val_loop: bits }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_finite_signal_evaluate() {
        let signal = FiniteSignal {
            values: vec![true, false, true],
        };

        assert_eq!(signal.evaluate(0), Ok(true));
        assert_eq!(signal.evaluate(1), Ok(false));
        assert_eq!(signal.evaluate(2), Ok(true));
        assert_eq!(signal.evaluate(3), Err(SignalErrors::SignalEnded));
    }

    #[test]
    fn test_infinite_signal_evaluate() {
        let signal = InfiniteSignal {
            val_loop: vec![true, false],
        };

        assert_eq!(signal.evaluate(0), Ok(true));
        assert_eq!(signal.evaluate(1), Ok(false));
        assert_eq!(signal.evaluate(2), Ok(true));
        assert_eq!(signal.evaluate(3), Ok(false));
        assert_eq!(signal.evaluate(4), Ok(true));

        for x in 0..100 {
            assert!(signal.evaluate(x as u64).is_ok());
        }
    }

    #[test]
    fn test_infinite_signal_repeating() {
        let pattern = "-_-";
        let signal = InfiniteSignal::repeating(pattern);
        assert!(signal.evaluate(0).unwrap());
        assert!(!signal.evaluate(1).unwrap());
        assert!(signal.evaluate(2).unwrap());
        assert!(signal.evaluate(3).unwrap());
        assert!(!signal.evaluate(4).unwrap());
        assert!(signal.evaluate(5).unwrap());

        let pattern = "-__-";
        let result = InfiniteSignal::repeating(pattern);
        assert!(result.evaluate(0).unwrap());
        assert!(!result.evaluate(1).unwrap());
        assert!(!result.evaluate(2).unwrap());
        assert!(result.evaluate(3).unwrap());
        assert!(result.evaluate(4).unwrap());
        assert!(!result.evaluate(5).unwrap());
    }

    #[test]
    fn test_finite_signal_embodiment() {
        let signal = FiniteSignal {
            values: vec![true, false, true],
        };

        let embodied = signal.embodiment("a".into());

        match embodied {
            ProceduralBlock::AlwaysBlock(_) => {
                panic!("Always block expected");
            }
            ProceduralBlock::InitialBlock(block) => {
                assert_eq!(block.statements.len(), 5);
            }
        }
    }

    #[test]
    fn test_infinite_signal_embodiment() {
        let signal = InfiniteSignal {
            val_loop: vec![true, false, true, false],
        };

        let embodied = signal.embodiment("a".into());
        match embodied {
            ProceduralBlock::InitialBlock(_) => {
                panic!("Always block expected");
            }
            ProceduralBlock::AlwaysBlock(block) => {
                assert_eq!(block.statements.len(), 8);
            }
        }
    }
}
