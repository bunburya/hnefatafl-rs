use std::hash::Hash;
#[cfg(test)]
use crate::tiles::Tile;

/// A stack that each value can only be pushed to once. Once a value has been pushed to the stack,
/// it cannot be pushed to the stack again, even if it has been popped (pushing again silently
/// fails).
#[derive(Default)]
pub(crate) struct UniqueStack<T: Hash + Eq + Copy> {
    stack: Vec<T>,
    added: std::collections::HashSet<T>
}

impl<T: Hash + Eq + Copy> UniqueStack<T> {
    pub(crate) fn push(&mut self, value: T) {
        if !self.added.contains(&value) {
            self.stack.push(value);
            self.added.insert(value);
        }
    }
    
    pub(crate) fn pop(&mut self) -> Option<T> {
        self.stack.pop()
    }
}

/// A queue of a fixed size. Pushing a new value to the end of the queue drops the first item in the
/// queue.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct FixedSizeQueue<T, const N: usize> {
    queue: [T; N],
    first_i: usize
}

impl<T, const N: usize> FixedSizeQueue<T, N> {

    #[cfg(test)]
    pub(crate) fn new(queue: [T; N]) -> Self {
        Self {
            queue,
            first_i: 0
        }
    }

    #[cfg(test)]
    fn last_i(&self) -> usize {
        if self.first_i == 0 {
            N - 1
        } else {
            self.first_i - 1
        }
    }

    #[cfg(test)]
    pub(crate) fn last(&self) -> &T {
        &self.queue[self.last_i()]
    }

    pub(crate) fn push(&mut self, value: T) {
        self.queue[self.first_i] = value;
        self.first_i = if self.first_i == N - 1 {
            0
        } else {
            self.first_i + 1
        }
    }

    pub(crate) fn first(&self) -> &T {
        &self.queue[self.first_i]
    }

}

impl<T: Default + Copy, const N: usize> Default for FixedSizeQueue<T, N> {
    fn default() -> Self {
        Self {
            queue: [T::default(); N],
            first_i: 0
        }
    }
}

#[cfg(test)]
/// Creates a [`std::collections::HashSet`] containing the arguments, similar to [`vec!`].
macro_rules! hashset {
    ($( $x: expr ),* ) => {
        {
            let mut tmp = std::collections::HashSet::new();
            $(
                tmp.insert($x);
            )*
            tmp
        }
    };
}

#[cfg(test)]
/// Assert that the given vector does not contain duplicates, and contains the same items as
/// a comparison vector (ignoring order).
pub(crate) fn check_tile_vec(actual: Vec<Tile>, expected: Vec<Tile>) {
    let actual_set: std::collections::HashSet<Tile> = actual.iter().copied().collect();
    assert_eq!(actual_set.len(), actual.len(), "Vec contains duplicates");
    let mut actual_sorted = actual.clone();
    actual_sorted.sort();
    let mut expected_sorted = expected.clone();
    expected_sorted.sort();
    assert_eq!(actual_sorted, expected_sorted);
}

#[cfg(test)]
mod tests {
    use crate::utils::FixedSizeQueue;

    #[test]
    fn test_fixed_queue() {
        let mut deque = FixedSizeQueue::new([1, 2, 3, 4, 5]);
        assert_eq!(*deque.first(), 1);
        assert_eq!(*deque.last(), 5);
        deque.push(99);
        assert_eq!(*deque.first(), 2);
        assert_eq!(*deque.last(), 99);
        deque.push(50);
        assert_eq!(*deque.first(), 3);
        assert_eq!(*deque.last(), 50);
    }
}