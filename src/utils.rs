#[cfg(test)]
use crate::tiles::Tile;
use std::hash::Hash;

/// A stack that each value can only be pushed to once. Once a value has been pushed to the stack,
/// it cannot be pushed to the stack again, even if it has been popped (pushing again silently
/// fails).
#[derive(Default)]
pub(crate) struct UniqueStack<T: Hash + Eq + Copy> {
    stack: Vec<T>,
    added: std::collections::HashSet<T>,
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
