use super::{FromBinary, ToBinary};
use crate::BytesWriter;
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::{BuildHasher, Hash},
    ptr::NonNull,
};

pub struct Decoder<'a> {
    start: NonNull<u8>,
    end: NonNull<u8>,
    eof: bool,
    error: Option<Cow<'static, str>>,
    _marker: std::marker::PhantomData<&'a ()>,
}

pub struct FromBinaryError {
    message: Cow<'static, str>,
}
impl FromBinaryError {
    pub fn message(&self) -> &str {
        &self.message
    }
}
impl std::fmt::Display for FromBinaryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::fmt::Debug for FromBinaryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl std::error::Error for FromBinaryError {}

impl<'a> Decoder<'a> {
    pub fn consume_error(&mut self) -> Option<FromBinaryError> {
        if self.eof {
            Some(FromBinaryError {
                message: Cow::Borrowed("Unexpected EOF"),
            })
        } else {
            self.error
                .take()
                .map(|error| FromBinaryError { message: error })
        }
    }
    pub fn new(slice: &'a [u8]) -> Self {
        let start = NonNull::new(slice.as_ptr() as *mut u8).unwrap();
        let end = NonNull::new(unsafe { slice.as_ptr().add(slice.len()) as *mut u8 }).unwrap();
        Self {
            start,
            end,
            eof: false,
            error: None,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<'a> Decoder<'a> {
    pub fn report_static_error(&mut self, error: &'static str) {
        if self.error.is_none() {
            self.error = Some(Cow::Borrowed(error));
            self.end = self.start;
        }
    }
    pub fn report_error(&mut self, error: std::fmt::Arguments) {
        if self.error.is_none() {
            self.error = Some(Cow::Owned(error.to_string()));
            self.end = self.start;
        }
    }
    fn byte_slice(&mut self, n: usize) -> &'a [u8] {
        unsafe {
            let nstart = self.start.add(n);
            if nstart.as_ptr() > self.end.as_ptr() {
                self.eof = true;
                return &[];
            }
            let ptr = std::slice::from_raw_parts(self.start.as_ptr(), n);
            self.start = nstart;
            ptr
        }
    }
    pub fn byte_array<const N: usize>(&mut self) -> &'a [u8; N] {
        unsafe {
            let nstart = self.start.add(N);
            if nstart > self.end {
                return &[0; N];
            }
            let ptr = self.start.as_ptr() as *const [u8; N];
            self.start = nstart;
            &*ptr
        }
    }
    pub fn byte(&mut self) -> u8 {
        unsafe {
            if self.start.as_ptr() >= self.end.as_ptr() {
                self.eof = true;
                return 0;
            }
            let value = *self.start.as_ptr();
            self.start = self.start.add(1);
            value
        }
    }
    pub fn read_length(&mut self) -> usize {
        unsafe {
            if self.start.as_ptr() >= self.end.as_ptr() {
                self.eof = true;
                return 0;
            }
            let value = *self.start.as_ptr();
            self.start = self.start.add(1);
            if value != 255 {
                value as usize
            } else {
                <u64 as FromBinary>::binary_decode(self) as usize
            }
        }
    }
}
pub fn write_length(encoder: &mut BytesWriter, len: usize) {
    if len >= 255 {
        encoder.push(255);
        encoder.push_bytes(&(len as u64).to_le_bytes());
    } else {
        encoder.push(len as u8);
    }
}

macro_rules! impl_bincode_for_numeric_primative {
    ($(( $ty:ty, $sz: tt )),* $(,)?) => {
        $(
            unsafe impl ToBinary for $ty {
                const POD: bool = true;
                fn binary_encode(&self, encoder: &mut BytesWriter) {
                    encoder.push_bytes(&self.to_le_bytes());
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    *self = self.to_le()
                }
            }
            unsafe impl<'a> FromBinary<'a> for $ty {
                const POD: bool = true;
                fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
                    unsafe {
                        let nstart = decoder.start.add($sz);
                        if nstart.as_ptr() > decoder.end.as_ptr() {
                            decoder.eof = true;
                            return 0;
                        }
                        let ptr = decoder.start.as_ptr() as *const [u8; $sz];
                        decoder.start = nstart;
                        <$ty>::from_le_bytes(*ptr)
                    }
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    *self = self.to_le()
                }
            }
        )*
    };
}

unsafe impl ToBinary for u8 {
    const POD: bool = true;
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        encoder.push(*self)
    }
}
unsafe impl<'a> FromBinary<'a> for u8 {
    const POD: bool = true;
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        decoder.byte()
    }
}

impl_bincode_for_numeric_primative! {
    (u16, 2),
    (u32, 4),
    (u64, 8),
    (u128, 16),
}

macro_rules! impl_bincode_via_transmute {
    ($( $src:ty as $reuse: ty ),* $(,)?) => {
        $(
            unsafe impl ToBinary for $src {
                const POD: bool = true;
                fn binary_encode(&self, encoder: &mut BytesWriter) {
                    <$reuse>::binary_encode(
                        unsafe { &*(self as *const _ as *const $reuse) },
                        encoder
                    )
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    <$reuse>::endian_transform(
                        unsafe { &mut *(self as *mut _ as *mut $reuse) }
                    )
                }
            }
            unsafe impl<'a> FromBinary<'a> for $src {
                const POD: bool = true;
                fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
                    unsafe {
                        std::mem::transmute::<$reuse, $src>(<$reuse>::binary_decode(decoder))
                    }
                }

                #[cfg(not(target_endian = "little"))]
                fn endian_transform(&mut self) {
                    <$reuse>::endian_transform(
                        unsafe { &mut *(self as *mut _ as *mut $reuse) }
                    )
                }
            }
        )*
    };
}

impl_bincode_via_transmute! {
    f32 as u32,
    f64 as u64,
    i8 as u8,
    i16 as u16,
    i32 as u32,
    i64 as u64,
    i128 as u128,
}

unsafe impl ToBinary for bool {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        encoder.push(*self as u8)
    }
}

unsafe impl<'a> FromBinary<'a> for bool {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        u8::binary_decode(decoder) == 1
    }
}

// Without length prefix
fn encode_array_body<T: ToBinary>(slice: &[T], encoder: &mut BytesWriter) {
    let can_memcopy = const { T::POD && (cfg!(target_endian = "little") || size_of::<T>() == 1) };
    if can_memcopy {
        let byte_size = std::mem::size_of_val(slice);
        unsafe {
            // TODO ensure size is small
            encoder.reserve_small(byte_size);
        }
        let initial_length = encoder.len();
        unsafe {
            std::ptr::copy_nonoverlapping(
                slice.as_ptr().cast::<u8>(),
                encoder.as_mut_ptr().add(initial_length),
                byte_size,
            );
            encoder.set_len(initial_length + byte_size);
        }
    } else {
        for value in slice {
            value.binary_encode(encoder)
        }
    }
}

unsafe impl<T: ToBinary> ToBinary for [T] {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        encode_array_body(self, encoder);
    }
}

unsafe impl<'a> FromBinary<'a> for &'a [u8] {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        decoder.byte_slice(len)
    }
}
unsafe impl ToBinary for String {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        self.as_bytes().binary_encode(encoder);
    }
}

unsafe impl ToBinary for Cow<'_, str> {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        self.as_bytes().binary_encode(encoder);
    }
}

unsafe impl<'a> FromBinary<'a> for Cow<'a, str> {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        Cow::Borrowed(<&'a str>::binary_decode(decoder))
    }
}
unsafe impl ToBinary for str {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        self.as_bytes().binary_encode(encoder);
    }
}
unsafe impl<'a> FromBinary<'a> for &'a str {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        match std::str::from_utf8(<&[u8]>::binary_decode(decoder)) {
            Ok(s) => s,
            Err(err) => {
                decoder.report_error(format_args!("Invalid Utf-8 string: {err:?}"));
                ""
            }
        }
    }
}

unsafe impl<'a, T: FromBinary<'a>, const N: usize> FromBinary<'a> for [T; N] {
    const POD: bool = T::POD;

    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        if T::POD {
            let byte_size = N * std::mem::size_of::<T>();
            let bytes = decoder.byte_slice(byte_size);
            if bytes.len() >= byte_size {
                let array = unsafe { std::ptr::read_unaligned::<[T; N]>(bytes.as_ptr().cast()) };

                #[cfg(not(target_endian = "little"))]
                for value in &mut array {
                    T::endian_transform(value);
                }
                return array;
            }
        }
        std::array::from_fn(|_| T::binary_decode(decoder))
    }

    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {
        if T::POD {
            for value in self {
                T::endian_transform(value);
            }
        }
    }
}
unsafe impl<T: ToBinary, const N: usize> ToBinary for [T; N] {
    const POD: bool = T::POD;

    fn binary_encode(&self, encoder: &mut BytesWriter) {
        encode_array_body(self, encoder)
    }

    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {
        if T::POD {
            for value in self {
                T::endian_transform(value);
            }
        }
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Vec<T> {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        if T::POD {
            let byte_size = len * std::mem::size_of::<T>();
            let bytes = decoder.byte_slice(byte_size);
            if bytes.len() >= byte_size {
                let mut vec: Vec<T> = Vec::with_capacity(len);
                unsafe {
                    std::ptr::copy_nonoverlapping::<u8>(
                        bytes.as_ptr(),
                        vec.as_mut_ptr().cast::<u8>(),
                        byte_size,
                    );
                    vec.set_len(len);
                }
                #[cfg(not(target_endian = "little"))]
                for value in &mut vec {
                    T::endian_transform(value);
                }
                vec
            } else {
                Vec::new()
            }
        } else {
            let mut vec = Vec::with_capacity(len.min(4096));
            for _ in 0..len {
                vec.push(T::binary_decode(decoder));
            }
            vec
        }
    }
}
unsafe impl<T: ToBinary> ToBinary for Vec<T> {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        <[T]>::binary_encode(self, encoder);
    }
}

unsafe impl<'a, T: ToBinary + ?Sized> ToBinary for &'a mut T {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        (**self).binary_encode(encoder)
    }
}

unsafe impl<'a, T: ToBinary + ?Sized> ToBinary for &'a T {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        (**self).binary_encode(encoder)
    }
}

unsafe impl<K: ToBinary, V: ToBinary, S> ToBinary for HashMap<K, V, S> {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for (k, v) in self {
            k.binary_encode(encoder);
            v.binary_encode(encoder);
        }
    }
}

unsafe impl<'a, K: FromBinary<'a> + Eq + Hash, V: FromBinary<'a>, S: BuildHasher + Default>
    FromBinary<'a> for HashMap<K, V, S>
{
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut map = HashMap::with_capacity_and_hasher(len.min(4096), Default::default());
        for _ in 0..len {
            let k = K::binary_decode(decoder);
            let v = V::binary_decode(decoder);
            map.insert(k, v);
        }
        map
    }
}

unsafe impl<K: ToBinary, S> ToBinary for HashSet<K, S> {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        write_length(encoder, self.len());
        for k in self {
            k.binary_encode(encoder);
        }
    }
}

unsafe impl<'a, K: FromBinary<'a> + Eq + Hash, S: BuildHasher + Default> FromBinary<'a>
    for HashSet<K, S>
{
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        let len = decoder.read_length();
        let mut map = HashSet::with_capacity_and_hasher(len.min(4096), Default::default());
        for _ in 0..len {
            map.insert(K::binary_decode(decoder));
        }
        map
    }
}

unsafe impl<T: ToBinary> ToBinary for Box<T> {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        T::binary_encode(self, encoder)
    }
}

unsafe impl<'a> FromBinary<'a> for String {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        <&str>::binary_decode(decoder).into()
    }
}

unsafe impl<'a> FromBinary<'a> for Box<str> {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        <&str>::binary_decode(decoder).into()
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Box<[T]> {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        <Vec<T>>::binary_decode(decoder).into()
    }
}

unsafe impl<T: ToBinary> ToBinary for Option<T> {
    fn binary_encode(&self, encoder: &mut BytesWriter) {
        if let Some(value) = self {
            encoder.push(1);
            value.binary_encode(encoder);
        } else {
            encoder.push(0);
        }
    }
}

unsafe impl<'a, T: FromBinary<'a>> FromBinary<'a> for Option<T> {
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self {
        if u8::binary_decode(decoder) == 1 {
            Some(T::binary_decode(decoder))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use crate::from_binary;
    use crate::BytesWriter;

    use super::*;
    #[derive(Default)]
    struct Tester<'a> {
        buffer: BytesWriter<'a>,
    }
    impl<'k> Tester<'k> {
        #[track_caller]
        fn assert_roundtrip<'a, T: FromBinary<'a> + ToBinary + PartialEq + Debug>(
            &'a mut self,
            value: &T,
        ) {
            unsafe {
                self.buffer.set_len(0);
            }
            value.binary_encode(&mut self.buffer);
            let result = crate::from_binary::<T>(self.buffer.buffer_slice()).unwrap();
            assert_eq!(value, &result);
        }
        #[track_caller]
        fn assert_roundtrip_all<T: for<'a> FromBinary<'a> + ToBinary + PartialEq + Debug>(
            &'_ mut self,
            values: &[T],
        ) {
            for value in values {
                self.assert_roundtrip(value);
            }
        }
    }
    #[test]
    fn numbers() {
        let mut tester = Tester::default();
        tester.assert_roundtrip_all(&[i8::MIN, 0, i8::MAX]);
        tester.assert_roundtrip_all(&[u8::MIN, 0, u8::MAX]);
        tester.assert_roundtrip_all(&[u16::MIN, 0, u16::MAX]);
        tester.assert_roundtrip_all(&[i16::MIN, 0, i16::MAX]);
        tester.assert_roundtrip_all(&[u32::MIN, 0, u32::MAX]);
        tester.assert_roundtrip_all(&[i32::MIN, 0, i32::MAX]);
        tester.assert_roundtrip_all(&[u64::MIN, 0, u64::MAX]);
        tester.assert_roundtrip_all(&[i64::MIN, 0, i64::MAX]);
        tester.assert_roundtrip_all(&[u128::MIN, 0, u128::MAX]);
        tester.assert_roundtrip_all(&[i128::MIN, 0, i128::MAX]);
        tester.assert_roundtrip_all(&[f32::MIN, 0.0, f32::MAX]);
        tester.assert_roundtrip_all(&[f64::MIN, 0.0, f64::MAX]);
    }

    #[test]
    fn strings() {
        let a = String::from_utf8(vec![b'a'; 254]).unwrap();
        let b = String::from_utf8(vec![b'a'; 255]).unwrap();
        let c = String::from_utf8(vec![b'a'; 256]).unwrap();
        let inputs = ["", "hello", &a, &b, &c];
        let encoded = inputs.map(|string| {
            let mut buf = BytesWriter::new();
            string.binary_encode(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<&str>(encoded).unwrap(), *input);
        }
        let mut output: BytesWriter = BytesWriter::new();
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Box<str> = (*input).into();
            boxed.binary_encode(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Box<str>>(&output.buffer_slice()).unwrap(),
                boxed
            );
        }
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: String = (*input).into();
            boxed.binary_encode(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<String>(&output.buffer_slice()).unwrap(),
                boxed
            );
        }
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Cow<'_, str> = (*input).into();
            boxed.binary_encode(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Cow<'_, str>>(&output.buffer_slice()).unwrap(),
                boxed
            );
        }
    }

    #[test]
    fn byte_arrays() {
        let a = vec![b'a'; 254];
        let b = vec![b'a'; 255];
        let c = vec![b'a'; 256];
        let inputs = [&b""[..], b"hello", &a, &b, &c];
        let encoded = inputs.map(|string| {
            let mut buf = BytesWriter::new();
            string.binary_encode(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<&[u8]>(encoded).unwrap(), *input);
        }
        let mut output = BytesWriter::new();
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Box<[u8]> = (*input).into();
            boxed.binary_encode(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Box<[u8]>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Vec<u8> = (*input).into();
            boxed.binary_encode(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Vec<u8>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
    }
    #[test]
    fn pod_arrays() {
        let a: Vec<u32> = (0..254u32)
            .map(|i| 0xdeadbeafu32.wrapping_mul(i) ^ i)
            .collect();
        let b = vec![b'a' as u32; 255];
        let c = vec![b'a' as u32; 256];
        let inputs = [&[][..], &[1u32, 2, 3, 4, 5], &a, &b, &c];
        let encoded = inputs.map(|string| {
            let mut buf = BytesWriter::new();
            string.binary_encode(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<Vec<u32>>(encoded).unwrap(), *input);
        }
        let mut output = BytesWriter::new();
        for (input, encoded) in inputs.iter().zip(&encoded) {
            output.clear();
            let boxed: Box<[u32]> = (*input).into();
            boxed.binary_encode(&mut output);
            assert_eq!(output.buffer_slice(), *encoded);
            assert_eq!(
                from_binary::<Box<[u32]>>(output.buffer_slice()).unwrap(),
                boxed
            );
        }
    }
    #[test]
    fn nested_pod_arrays() {
        let inputs: [&[[u32; 2]]; 2] = [&[][..], &[[1u32, 2], [3, 4], [5, 3]]];
        let encoded = inputs.map(|array| {
            let mut buf = BytesWriter::new();
            array.binary_encode(&mut buf);
            buf.into_vec()
        });
        for (input, encoded) in inputs.iter().zip(&encoded) {
            assert_eq!(from_binary::<Vec<[u32; 2]>>(encoded).unwrap(), *input);
        }
    }
    #[test]
    fn complex_arrays() {
        let inputs: &[&str] = &["hello", "", "nice"];
        let encoded = crate::to_binary(inputs);
        assert_eq!(&from_binary::<Vec<&str>>(&encoded).unwrap(), inputs)
    }
}
