use crate::parsers::behavior::{AlwaysBlock, InitialBlock};

enum ExecutionCursor {
    // next statement to execute, and the block to execute it in
    InitialCursor((usize, InitialBlock)),
    AlwaysCursor((usize, AlwaysBlock)),
}

pub struct EventQueue {
    // The next exection time, and the cursor that execution will be resumed at
    cursors: Vec<ExecutionCursor>,
    times: Vec<i64>,
}

impl EventQueue {
    pub fn new() -> Self {
        EventQueue {
            cursors: Vec::new(),
            times: Vec::new(),
        }
    }

    pub fn insert(&mut self, time: i64, cursor: ExecutionCursor) {
        let position = self
            .times
            .binary_search(&time);

        let position = match position {
            // Found a match, so we have to search for the last match
            Ok(position) => {
                let mut last_match = position;
                while last_match < self.times.len() && self.times[last_match] == time {
                    last_match += 1;
                }
                last_match
            },
            // No match, so insert is default safe
            Err(position) => position,
        };

        // Insert the time and cursor
        self.cursors.insert(position,  cursor);
        self.times.insert(position, time);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_queue_new() {
        let queue = EventQueue::new();
        assert!(queue.cursors.is_empty());
    }

    #[test]
    fn test_event_queue_insert() {
        let mut queue = EventQueue::new();
        let initial_block = InitialBlock::new(vec![]); // Assuming InitialBlock has a default constructor
        let always_block = AlwaysBlock::new(vec![], vec![]); // Assuming AlwaysBlock has a default constructor

        queue.insert(10, ExecutionCursor::InitialCursor((0, initial_block)));
        queue.insert(5, ExecutionCursor::AlwaysCursor((1, always_block)));

        assert_eq!(queue.cursors.len(), 2);
        assert_eq!(queue.times[0], 5);
        assert_eq!(queue.times[1], 10);
    }

    #[test]
    fn test_event_queue_insert_same_time() {
        let mut queue = EventQueue::new();
        let initial_block1 = InitialBlock::new(vec![]);
        let initial_block2 = InitialBlock::new(vec![]);

        queue.insert(10, ExecutionCursor::InitialCursor((0, initial_block1)));
        queue.insert(10, ExecutionCursor::InitialCursor((1, initial_block2)));

        assert_eq!(queue.cursors.len(), 2);
        assert_eq!(queue.times[0], 10);
        assert_eq!(queue.times[1], 10);
    }

    fn test_event_queue_insert_stable() {

    }

}
