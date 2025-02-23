use crate::parsers::behavior::{AlwaysBlock, InitialBlock};


enum ExecutionCursor {
    // next statement to execute, and the block to execute it in
    InitialCursor((usize, InitialBlock)),
    AlwaysCursor((usize, AlwaysBlock)),
}

pub struct EventQueue {
    // The next exection time, and the cursor that execution will be resumed at
    cursors: Vec<(i64, ExecutionCursor)>,
}

impl EventQueue {
    pub fn new() -> Self {
        EventQueue {
            cursors: Vec::new(),
        }
    }

    pub fn insert(&mut self, time: i64, cursor: ExecutionCursor) {
        // TODO(meawoppl) - test insert stability
        let position = self.cursors.binary_search_by_key(&time, |&(t, _)| t)
            .unwrap_or_else(|e| e);
        self.cursors.insert(position, (time, cursor));
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
        assert_eq!(queue.cursors[0].0, 5);
        assert_eq!(queue.cursors[1].0, 10);
    }

    #[test]
    fn test_event_queue_insert_same_time() {
        let mut queue = EventQueue::new();
        let initial_block1 = InitialBlock::new(vec![]); 
        let initial_block2 = InitialBlock::new(vec![]);

        queue.insert(10, ExecutionCursor::InitialCursor((0, initial_block1)));
        queue.insert(10, ExecutionCursor::InitialCursor((1, initial_block2)));

        assert_eq!(queue.cursors.len(), 2);
        assert_eq!(queue.cursors[0].0, 10);
        assert_eq!(queue.cursors[1].0, 10);
    }
}
