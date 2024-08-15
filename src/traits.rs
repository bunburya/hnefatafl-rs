use crate::tiles::Tile;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not, Shl};

/// A trait for any integer type that can be used as a bitfield to store board state. See also the
/// [`crate::impl_bitfield!`] macro that can help to implement this trait for a particular integer
/// type.
pub trait BitField:
    Sized +
    Copy +
    From<u8> +
    BitAnd<Output=Self> +
    BitAndAssign +
    BitOr<Output=Self> +
    BitOrAssign +
    Not<Output=Self> +
    Shl<Output=Self> +
    PartialOrd +
    Default
{
    /// The type that is returned by `Self::to_be_bytes` and accepted by `Self::from_be_bytes`.
    /// In general this should be of the form `[u8; n]` where `n` is the size in bytes of the
    /// integer type.
    type Bytes: AsRef<[u8]> + AsMut<[u8]> + Default;
    
    /// The number of bits used to represent a single row of the board. This number should, of
    /// course, be at least equal to the board length. It should be less than the square root
    /// of the number of bits in the integer type. This is so that, after representing the entire
    /// board, there are enough bits left over to store the position of the king.
    const ROW_WIDTH: u8;

    /// Returns the number of ones in the binary representation of `self`.
    fn count_ones(self) -> u32;
    
    /// Return the memory representation of this integer as a byte array in big-endian (network)
    /// byte order.
    fn to_be_bytes(self) -> Self::Bytes;
    
    /// Create an integer value from its representation as a byte array in big endian.
    fn from_be_bytes(bytes: Self::Bytes) -> Self;
    
    /// Create an integer value from a big endian byte array slice.
    fn from_be_bytes_slice(bytes: &[u8]) -> Self {
        let mut new_bytes = Self::Bytes::default();
        new_bytes.as_mut().copy_from_slice(bytes.as_ref());
        Self::from_be_bytes(new_bytes)
    }

    /// Create a bitmask for the given tile. Only the bit corresponding to the tile's position on
    /// the board will be set.
    fn tile_mask(t: Tile) -> Self {
        Self::from(1) << ((t.row() * Self::ROW_WIDTH) + t.col()).into()
    }

}

/// Implement the [BitField] trait for the given integer type. First argument should be the type
/// to implement the trait for; the second should be the byte value to use for
/// [BitField::ROW_WIDTH].
#[macro_export] macro_rules! impl_bitfield {
    ($t:ty, $row_width:expr) => {
        impl BitField for $t {
            type Bytes = [u8; size_of::<$t>()];
            const ROW_WIDTH: u8 = $row_width;

            fn count_ones(self) -> u32 {
                <$t>::count_ones(self)
            }
            
            fn to_be_bytes(self) -> Self::Bytes {
                <$t>::to_be_bytes(self)
            }

            fn from_be_bytes(bytes: Self::Bytes) -> Self {
                <$t>::from_be_bytes(bytes)
            }
        }
    };
}

impl_bitfield!(u64, 7);
impl_bitfield!(u128, 11);
