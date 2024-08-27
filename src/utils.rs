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
