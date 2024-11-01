use std::collections::HashSet;
use std::hash::Hash;

/// Creates a [`HashSet`] containing the arguments, similar to [`vec!`].
#[macro_export] macro_rules! hashset {
    ($( $x: expr ),* ) => {
        {
            let mut tmp = HashSet::new();
            $(
                tmp.insert($x);
            )*
            tmp
        }
    };
}

/// A stack that each value can only be pushed to once. Once a value has been pushed to the stack,
/// it cannot be pushed to the stack again, even if it has been popped.
#[derive(Default)]
pub(crate) struct UniqueStack<T: Hash + Eq + Copy> {
    stack: Vec<T>,
    added: HashSet<T>
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