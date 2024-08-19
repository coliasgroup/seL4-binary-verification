pub(crate) trait DoubleEndedIteratorExt: DoubleEndedIterator {
    fn foldr1_like_gr<F>(mut self, mut f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(Self::Item, Self::Item) -> Self::Item,
    {
        let last = self.next_back()?;
        Some(self.rfold(last, |acc, x| f(x, acc)))
    }
}

impl<T> DoubleEndedIteratorExt for T where T: DoubleEndedIterator + ?Sized {}
