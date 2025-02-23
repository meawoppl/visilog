use std::collections::HashMap;
use std::fmt;

pub struct Register {
    bits: Vec<u8>
}

pub struct StateStore {
    name_to_register: HashMap<String, Register>
}

impl StateStore {
    pub fn new() -> Self {
        StateStore {
            name_to_register: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, register: Register) {
        self.name_to_register.insert(name, register);
    }
}


impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut keys: Vec<&String> = self.name_to_register.keys().collect();
        keys.sort();
        for key in keys {
            writeln!(f, "{}: {:?}", key, self.name_to_register.get(key).unwrap().bits)?;
        }
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_display() {
        let mut state_store = StateStore {
            name_to_register: HashMap::new(),
        };

        state_store.name_to_register.insert(
            "reg1".to_string(),
            Register { bits: vec![1, 0, 1] },
        );
        state_store.name_to_register.insert(
            "reg2".to_string(),
            Register { bits: vec![0, 1, 0] },
        );

        let display_output = format!("{}", state_store);
        let expected_output = "reg1: [1, 0, 1]\nreg2: [0, 1, 0]\n";

        assert_eq!(display_output, expected_output);
    }

    #[test]
    fn test_insert() {
        let mut state_store = StateStore::new();
        let register = Register { bits: vec![1, 1, 1] };
        state_store.insert("reg1".to_string(), register);

        assert!(state_store.name_to_register.contains_key("reg1"));
        assert_eq!(state_store.name_to_register.get("reg1").unwrap().bits, vec![1, 1, 1]);
    }

    #[test]
    fn test_insert_multiple() {
        let mut state_store = StateStore::new();
        state_store.insert("reg1".to_string(), Register { bits: vec![1, 0, 1] });
        state_store.insert("reg2".to_string(), Register { bits: vec![0, 1, 0] });

        assert!(state_store.name_to_register.contains_key("reg1"));
        assert!(state_store.name_to_register.contains_key("reg2"));
        assert_eq!(state_store.name_to_register.get("reg1").unwrap().bits, vec![1, 0, 1]);
        assert_eq!(state_store.name_to_register.get("reg2").unwrap().bits, vec![0, 1, 0]);
    }

    #[test]
    fn test_insert_overwrite() {
        let mut state_store = StateStore::new();
        state_store.insert("reg1".to_string(), Register { bits: vec![1, 0, 1] });
        state_store.insert("reg1".to_string(), Register { bits: vec![0, 0, 0] });

        assert!(state_store.name_to_register.contains_key("reg1"));
        assert_eq!(state_store.name_to_register.get("reg1").unwrap().bits, vec![0, 0, 0]);
    }
}

