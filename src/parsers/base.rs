pub trait RawToken: Sized {
    fn raw_token(&self) -> String;
}
