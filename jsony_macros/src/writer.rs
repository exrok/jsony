mod cache;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};

enum IdentCacheEntry {
    Empty(&'static str),
    Cached(Ident),
}

pub struct RustWriter {
    pub buf: Vec<TokenTree>,
    cache: Cache,
}

/// Reduces macro evaluation time by 10% on rustc 1.82
struct Cache {
    default_span: Span,
    ident: Box<[IdentCacheEntry; cache::IDENT_SIZE]>,
    punct: [Punct; cache::PUNCT_SIZE],
}

impl Cache {
    #[allow(dead_code)]
    fn token_from_index(&mut self, index: usize) -> TokenTree {
        if let Some(punct) = self.punct.get(index) {
            return TokenTree::Punct(punct.clone());
        }
        if let Some(ident) = self.ident.get_mut(index - self.punct.len()) {
            match ident {
                IdentCacheEntry::Empty(value) => {
                    let re = Ident::new(value, self.default_span);
                    let ret = re.clone();
                    *ident = IdentCacheEntry::Cached(re);
                    return TokenTree::Ident(ret);
                }
                IdentCacheEntry::Cached(ident) => return TokenTree::Ident(ident.clone()),
            }
        }
        let idx = index - self.punct.len() - self.ident.len();
        let del = match idx {
            0 => Delimiter::Parenthesis,
            1 => Delimiter::Brace,
            _ => Delimiter::Bracket,
        };
        TokenTree::Group(Group::new(del, TokenStream::new()))
    }
}

impl RustWriter {
    pub fn new() -> Self {
        RustWriter {
            buf: Vec::new(),
            cache: Cache {
                default_span: Span::mixed_site(),
                ident: cache::ident_cache_initial_state(),
                punct: cache::punct_cache_initial_state(),
            },
        }
    }
    #[inline(never)]
    pub fn tt_group(&mut self, delimiter: Delimiter, from: usize) {
        let group = TokenTree::Group(Group::new(
            delimiter,
            TokenStream::from_iter(self.buf.drain(from..)),
        ));
        self.buf.push(group);
    }
    pub fn split_off_stream(&mut self, from: usize) -> TokenStream {
        TokenStream::from_iter(self.buf.drain(from..))
    }
    pub fn tt_group_empty(&mut self, delimiter: Delimiter) {
        let group = TokenTree::Group(Group::new(delimiter, TokenStream::new()));
        self.buf.push(group);
    }
    #[allow(dead_code)]
    #[inline(never)]
    pub fn blit(&mut self, start: u32, len: u32) {
        let start = start as usize;
        let len = len as usize;
        let src = &cache::BLIT_SRC[start..start + len];
        self.buf
            .extend(src.iter().map(|i| self.cache.token_from_index(*i as usize)));
    }
    #[allow(dead_code)]
    pub fn blit_punct(&mut self, index: usize) {
        self.buf
            .push(TokenTree::Punct(self.cache.punct[index].clone()));
    }

    #[allow(dead_code)]
    #[inline(never)]
    pub fn blit_ident(&mut self, index: usize) {
        let entry = &mut self.cache.ident[index];
        match entry {
            IdentCacheEntry::Empty(name) => {
                let ident = Ident::new(name, self.cache.default_span);
                self.buf.push(TokenTree::Ident(ident.clone()));
                *entry = IdentCacheEntry::Cached(ident);
            }
            IdentCacheEntry::Cached(ident) => self.buf.push(TokenTree::Ident(ident.clone())),
        }
    }
}
