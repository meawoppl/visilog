use std::collections::VecDeque;

use crate::parsers::behavior::{AlwaysBlock, InitialBlock};

pub enum ExecutionCursor {
    // next statement to execute, and the block to execute it in
    InitialCursor((usize, InitialBlock)),
    AlwaysCursor((usize, AlwaysBlock)),
}

/// Cursors ordered by the time they are due to resume. Cursors queued for the
/// same time come back out in the order they went in.
pub struct EventQueue {
    entries: VecDeque<(i64, ExecutionCursor)>,
}

impl EventQueue {
    pub fn new() -> Self {
        EventQueue {
            entries: VecDeque::new(),
        }
    }

    pub fn insert(&mut self, time: i64, cursor: ExecutionCursor) {
        // Insert past every entry already due at this time, which is what keeps
        // equal timestamps first-in-first-out.
        let position = self.entries.partition_point(|(queued, _)| *queued <= time);
        self.entries.insert(position, (time, cursor));
    }

    /// Remove and return the earliest cursor, along with the time it is due.
    pub fn pop(&mut self) -> Option<(i64, ExecutionCursor)> {
        self.entries.pop_front()
    }

    /// The time of the earliest cursor, without consuming it.
    pub fn peek_time(&self) -> Option<i64> {
        self.entries.front().map(|(time, _)| *time)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsers::behavior::EventControl;

    fn initial_cursor() -> ExecutionCursor {
        ExecutionCursor::InitialCursor((0, InitialBlock::new(vec![])))
    }

    fn always_cursor() -> ExecutionCursor {
        ExecutionCursor::AlwaysCursor((0, AlwaysBlock::new(EventControl::None, vec![])))
    }

    #[test]
    fn test_event_queue_new() {
        let queue = EventQueue::new();
        assert!(queue.is_empty());
        assert_eq!(queue.len(), 0);
        assert_eq!(queue.peek_time(), None);
    }

    #[test]
    fn test_event_queue_insert() {
        let mut queue = EventQueue::new();
        let initial_block = InitialBlock::new(vec![]);
        let always_block = AlwaysBlock::new(EventControl::None, vec![]);

        queue.insert(10, ExecutionCursor::InitialCursor((0, initial_block)));
        queue.insert(5, ExecutionCursor::AlwaysCursor((1, always_block)));

        assert_eq!(queue.len(), 2);
        assert_eq!(queue.entries[0].0, 5);
        assert_eq!(queue.entries[1].0, 10);
    }

    #[test]
    fn test_event_queue_insert_same_time() {
        let mut queue = EventQueue::new();

        queue.insert(10, initial_cursor());
        queue.insert(10, initial_cursor());

        for _ in 0..10 {
            queue.insert(10, always_cursor());

            match &queue.entries.back().unwrap().1 {
                ExecutionCursor::AlwaysCursor((_, _)) => {}
                _ => panic!("Expected AlwaysBlock"),
            }

            queue.insert(10, initial_cursor());

            match &queue.entries.back().unwrap().1 {
                ExecutionCursor::InitialCursor(_) => {}
                _ => panic!("Expected InitialBlock"),
            }
        }
    }

    #[test]
    fn test_pop_returns_cursors_in_time_order() {
        let mut queue = EventQueue::new();

        queue.insert(30, initial_cursor());
        queue.insert(10, initial_cursor());
        queue.insert(20, initial_cursor());

        let times: Vec<i64> = std::iter::from_fn(|| queue.pop())
            .map(|(time, _)| time)
            .collect();

        assert_eq!(times, vec![10, 20, 30]);
        assert!(queue.is_empty());
    }

    #[test]
    fn test_pop_is_fifo_within_one_time() {
        let mut queue = EventQueue::new();

        queue.insert(10, initial_cursor());
        queue.insert(10, always_cursor());
        queue.insert(10, initial_cursor());
        // An earlier time inserted last still comes out first.
        queue.insert(5, always_cursor());

        let kinds: Vec<(i64, bool)> = std::iter::from_fn(|| queue.pop())
            .map(|(time, cursor)| (time, matches!(cursor, ExecutionCursor::InitialCursor(_))))
            .collect();

        assert_eq!(kinds, vec![(5, false), (10, true), (10, false), (10, true)]);
    }

    #[test]
    fn test_peek_time_does_not_consume() {
        let mut queue = EventQueue::new();
        queue.insert(7, initial_cursor());
        queue.insert(3, always_cursor());

        assert_eq!(queue.peek_time(), Some(3));
        assert_eq!(queue.peek_time(), Some(3));
        assert_eq!(queue.len(), 2);

        assert_eq!(queue.pop().unwrap().0, 3);
        assert_eq!(queue.peek_time(), Some(7));
    }

    #[test]
    fn test_pop_empty_queue() {
        let mut queue = EventQueue::new();
        assert!(queue.pop().is_none());

        queue.insert(1, initial_cursor());
        assert!(queue.pop().is_some());
        assert!(queue.pop().is_none());
        assert!(queue.is_empty());
    }

    #[test]
    fn test_negative_times_are_ordered() {
        let mut queue = EventQueue::new();
        queue.insert(0, initial_cursor());
        queue.insert(-5, initial_cursor());
        queue.insert(5, initial_cursor());

        let times: Vec<i64> = std::iter::from_fn(|| queue.pop())
            .map(|(time, _)| time)
            .collect();
        assert_eq!(times, vec![-5, 0, 5]);
    }
}
