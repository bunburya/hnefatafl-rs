use crate::tiles::Tile;
use primitive_types::{U256, U512};
use std::fmt::Debug;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not, Shl, Shr};

/// A very simple trait for numeric array types, giving them a `zero` method that returns an array
/// of zeroes. Used for the `Bytes` associated type of the [`BitField`] trait, as not all byte
/// arrays implement [`Default`].
pub trait ZeroArray {
    fn zero() -> Self;
}

macro_rules! impl_zero_array {
    ($t:ty) => {
        impl ZeroArray for [u8; size_of::<$t>()] {
            fn zero() -> Self {
                [0; size_of::<$t>()]
            }
        }
    }
}

/// A trait for any integer type that can be used as a bitfield to store board state. See also the
/// [`crate::impl_bitfield!`] and [`crate::impl_bitfield_bigint!`] macros that can help to implement
/// this trait for a particular integer type.
pub trait BitField:
    Sized +
    Copy +
    From<u8> +
    BitAnd<Output=Self> +
    BitAndAssign +
    BitOr<Output=Self> +
    BitOrAssign +
    Not<Output=Self> +
    Shr<u32, Output=Self> +
    Shl<u32, Output=Self> +
    PartialOrd +
    PartialEq +
    Default +
    Debug
{
    /// The type that is returned by `Self::to_be_bytes` and accepted by `Self::from_be_bytes`.
    /// In general this should be of the form `[u8; n]` where `n` is the size in bytes of the
    /// integer type.
    type Bytes: AsRef<[u8]> + AsMut<[u8]> + ZeroArray;
    
    /// The number of bits used to represent a single row of the board. This number should, of
    /// course, be at least equal to the board length. It should be less than the square root
    /// of the number of bits in the integer type. This is so that, after representing the entire
    /// board, there are enough bits left over to store the position of the king.
    const ROW_WIDTH: u8;

    /// Returns the number of ones in the binary representation of `self`.
    fn count_ones(&self) -> u32;
    
    /// Return the memory representation of this integer as a byte array in big-endian (network)
    /// byte order.
    fn to_be_bytes(&self) -> Self::Bytes;
    
    /// Create an integer value from its representation as a byte array in big endian.
    fn from_be_bytes(bytes: Self::Bytes) -> Self;
    
    /// Create an integer value from a big endian byte array slice.
    fn from_be_bytes_slice(bytes: &[u8]) -> Self {
        let mut new_bytes = Self::Bytes::zero();
        new_bytes.as_mut().copy_from_slice(bytes.as_ref());
        Self::from_be_bytes(new_bytes)
    }

    /// Create a bitmask for the given tile. Only the bit corresponding to the tile's position on
    /// the board will be set.
    fn tile_mask(t: Tile) -> Self {
        Self::from(1) << ((t.row * Self::ROW_WIDTH) + t.col).into()
    }
    
    /// Covert the given bit index to a tile.
    fn bit_to_tile(bit: u32) -> Tile {
        let row = bit / (Self::ROW_WIDTH as u32);
        let col = bit - (row * (Self::ROW_WIDTH as u32));
        Tile { row: row as u8, col: col as u8 }
    }
    
    /// Return the number of trailing zeros in the bitfield.
    fn trailing_zeros(&self) -> u32;

    /// Return the number of leading zeros in the bitfield.
    fn leading_zeros(&self) -> u32;
    
    /// Whether the bitfield is empty (ie, no set bits).
    fn is_empty(&self) -> bool;

}

/// Implement the [`BitField`] trait for the given integer type. First argument should be the type
/// to implement the trait for; the second should be the byte value to use for
/// [`BitField::ROW_WIDTH`]. This macro is for use with the standard library integer types.
#[macro_export] macro_rules! impl_bitfield {
    ($t:ty, $row_width:expr) => {

        impl_zero_array!($t);

        impl BitField for $t {
            type Bytes = [u8; size_of::<$t>()];
            const ROW_WIDTH: u8 = $row_width;

            fn count_ones(&self) -> u32 {
                <$t>::count_ones(*self)
            }

            fn to_be_bytes(&self) -> Self::Bytes {
                <$t>::to_be_bytes(*self)
            }

            fn from_be_bytes(bytes: Self::Bytes) -> Self {
                <$t>::from_be_bytes(bytes)
            }

            fn trailing_zeros(&self) -> u32 {
                <$t>::trailing_zeros(*self)
            }

            fn leading_zeros(&self) -> u32 {
                <$t>::leading_zeros(*self)
            }

            fn is_empty(&self) -> bool {
                *self == 0
            }
        }
    };
}


/// Implement the [`BitField`] trait for the given integer type. First argument should be the type
/// to implement the trait for; the second should be the byte value to use for
/// [`BitField::ROW_WIDTH`]. This macro is for use with the big integer types provided by the
/// [`primitive_types`] crate. It implements the trait in a way that works with the methods exposed
/// by these types. Trying to use this macro on other types (or, conversely, trying to use the
/// [`crate::impl_bitfield!`] macro on the `primitive_types` types) could result in weird and
/// difficult to debug errors like stack overflows.
#[macro_export] macro_rules! impl_bitfield_bigint {
    ($t:ty, $row_width:expr) => {

        impl_zero_array!($t);

        impl BitField for $t {
            type Bytes = [u8; size_of::<$t>()];
            const ROW_WIDTH: u8 = $row_width;

            fn count_ones(&self) -> u32 {
                self.to_be_bytes().iter().map(|b| b.count_ones()).sum()
            }
            
            fn to_be_bytes(&self) -> Self::Bytes {
                <$t>::to_big_endian(self)
            }

            fn from_be_bytes(bytes: Self::Bytes) -> Self {
                <$t>::from_big_endian(&bytes)
            }
            
            fn trailing_zeros(&self) -> u32 {
                <$t>::trailing_zeros(self)
            }
            
            fn leading_zeros(&self) -> u32 {
                <$t>::leading_zeros(self)
            }
            
            fn is_empty(&self) -> bool {
                *self == Self::zero()
            }
        }
    };
}


impl_bitfield!(u64, 7);
impl_bitfield!(u128, 11);
impl_bitfield_bigint!(U256, 15);
impl_bitfield_bigint!(U512, 21);