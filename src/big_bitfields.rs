use crate::bitfield::ZeroArray;
use std::fmt::Formatter;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr};

#[cfg(feature = "serde")]
use serde::{
    de::{Error, SeqAccess, Visitor}, ser::SerializeTuple, Deserialize, Deserializer,
    Serialize,
    Serializer
};

#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
struct ByteArray<const N: usize>([u8; N]);

#[cfg(feature = "serde")]
impl<const N: usize> Serialize for ByteArray<N> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut s = serializer.serialize_tuple(N)?;
        for b in self.0 {
            s.serialize_element(&b)?;
        }
        s.end()
    }
}

#[cfg(feature = "serde")]
struct ByteArrayVisitor<const N: usize>;

#[cfg(feature = "serde")]
impl<'de, const N: usize> Visitor<'de> for ByteArrayVisitor<N> {
    type Value = ByteArray<N>;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str(&format!("a byte array of length {N}"))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
        let mut data = [0u8; N];
        for (i, byte) in data.iter_mut().enumerate() {
            *byte = seq.next_element()?
                .ok_or_else(|| A::Error::invalid_length(i, &self))?;
        }
        Ok(ByteArray(data))
    }
}

#[cfg(feature = "serde")]
impl<'de, const N: usize> Deserialize<'de> for ByteArray<N> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_tuple(N, ByteArrayVisitor::<N>)
    }
}


/// Support for arbitrarily large bitfields. The const parameter `N` specifies the number of bytes
/// in the bitfield.
///
/// **NOTE**: Does not support integer arithmetic. Only supports the bitwise operations necessary
/// to represent board state.
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BigBitfield<const N: usize> {
    bytes: ByteArray<N>
}

impl<const N: usize> BigBitfield<N> {

    pub const fn from_be_bytes(bytes: [u8; N]) -> Self {
        Self { bytes: ByteArray(bytes) }
    }

    pub fn to_be_bytes(self) -> [u8; N] {
        self.bytes.0
    }

}

impl<const N: usize> ZeroArray for BigBitfield<N> {
    fn zero() -> Self {
        Self { bytes: ByteArray([0u8; N]) }
    }
}

impl<const N: usize> From<u8> for BigBitfield<N> {
    fn from(n: u8) -> Self {
        let mut bytes = [0u8; N];
        bytes[N-1] = n;
        Self { bytes: ByteArray(bytes) }
    }
}

impl<const N: usize> Default for BigBitfield<N> {
    fn default() -> Self {
        Self::zero()
    }
}

impl<const N: usize> BitAnd for BigBitfield<N> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        let mut words = [0u8; N];
        for i in 0..N {
            words[i] = self.bytes.0[i] & rhs.bytes.0[i];
        }
        Self { bytes: ByteArray(words) }
    }
}

impl<const N: usize> BitAndAssign for BigBitfield<N> {
    fn bitand_assign(&mut self, rhs: Self) {
        for i in 0..N {
            self.bytes.0[i] &= rhs.bytes.0[i];
        }
    }
}

impl<const N: usize> BitOr for BigBitfield<N> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        let mut words = [0u8; N];
        for i in 0..N {
            words[i] = self.bytes.0[i] | rhs.bytes.0[i];
        }
        Self { bytes: ByteArray(words) }
    }
}

impl<const N: usize> BitOrAssign for BigBitfield<N> {
    fn bitor_assign(&mut self, rhs: Self) {
        for i in 0..N {
            self.bytes.0[i] |= rhs.bytes.0[i];
        }
    }
}

impl<const N: usize> BitXor for BigBitfield<N> {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        let mut words = [0u8; N];
        for i in 0..N {
            words[i] = self.bytes.0[i] ^ rhs.bytes.0[i];
        }
        Self { bytes: ByteArray(words) }
    }
}

impl<const N: usize> BitXorAssign for BigBitfield<N> {
    fn bitxor_assign(&mut self, rhs: Self) {
        for i in 0..N {
            self.bytes.0[i] ^= rhs.bytes.0[i];
        }
    }
}

impl<const N: usize> Not for BigBitfield<N> {
    type Output = Self;
    fn not(self) -> Self {
        let mut words = [0u8; N];
        for i in 0..N {
            words[i] = !self.bytes.0[i];
        }
        Self { bytes: ByteArray(words) }
    }
}

impl<const N: usize> Shl<u32> for BigBitfield<N> {
    type Output = Self;
    fn shl(self, bits: u32) -> Self::Output {
        if bits >= (N * 8) as u32 {
            return Self::zero();
        }
        let byte_shift = (bits / 8) as usize;
        let bit_shift = bits % 8;
        let mut result = [0u8; N];
        if bit_shift == 0 {
            // Only byte shift: move bytes toward lower indices (left)
            result[..(N - byte_shift)].copy_from_slice(
                &self.bytes.0[byte_shift..N]
            );
        } else {
            for i in 0..(N - byte_shift) {
                // Shift current byte left
                result[i] = self.bytes.0[i + byte_shift] << bit_shift;

                // Get overflow from next byte (to the right, higher index)
                if i + byte_shift + 1 < N {
                    result[i] |= self.bytes.0[i + byte_shift + 1] >> (8 - bit_shift);
                }
            }
        }

        Self { bytes: ByteArray(result) }
    }
}

impl<const N: usize> Shr<u32> for BigBitfield<N> {
    type Output = Self;
    fn shr(self, bits: u32) -> Self::Output {
        if bits >= (N * 8) as u32 {
            return Self::zero();
        }
        let byte_shift = (bits / 8) as usize;
        let bit_shift = bits % 8;
        let mut result = [0u8; N];
        if bit_shift == 0 {
            // Only byte shift: move bytes toward higher indices (right)
            result[byte_shift..N].copy_from_slice(&self.bytes.0[..(N - byte_shift)]);
        } else {
            // Byte and bit shift
            for i in byte_shift..N {
                // Shift current byte right
                result[i] = self.bytes.0[i - byte_shift] >> bit_shift;

                // Get overflow from previous byte (to the left, lower index)
                if i > byte_shift {
                    result[i] |= self.bytes.0[i - byte_shift - 1] << (8 - bit_shift);
                }
            }
        }
        Self { bytes: ByteArray(result) }
    }
}

pub type B256 = BigBitfield<32>;
pub type B512 = BigBitfield<64>;

#[cfg(test)]
mod tests {
    use crate::big_bitfields::BigBitfield;

    #[test]
    fn test_bbf() {
        let n = 13543124514323414u64;
        let b = n.to_be_bytes() as [u8; 8];
        let bf1 = BigBitfield::from_be_bytes(b);
        assert_eq!(bf1.bytes.0, b);

        let x = 567438294274u64;
        let bf2 = BigBitfield::from_be_bytes(x.to_be_bytes() as [u8; 8]);

        assert_eq!(
            (bf1 ^ bf2).bytes.0,
            (n ^ x).to_be_bytes()
        );
        assert_eq!(
            (bf1 & bf2).bytes.0,
            (n & x).to_be_bytes()
        );
        assert_eq!(
            (bf1 | bf2).bytes.0,
            (n | x).to_be_bytes()
        );
        assert_eq!(
            (bf1 << 42).bytes.0,
            (n << 42).to_be_bytes()
        );
        assert_eq!(
            (bf1 >> 42).bytes.0,
            (n >> 42).to_be_bytes()
        )
    }
}