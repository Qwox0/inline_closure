# `inline_closure!` macro

[![Github](https://img.shields.io/badge/github-Qwox0/inline__closure-blue?style=flat&logo=github)](https://github.com/Qwox0/inline_closure)

A Rust macro that inlines closures. This may slightly improve performance and is especially useful in other macros:

# Example

```rust
# use inline_closure::inline_closure;

trait MyTrait {
    fn create(val: i32) -> Self;
}

macro_rules! impl_trait {
    ($($ty:ty)* : $func:expr) => { $(
        impl MyTrait for $ty {
            fn create(val: i32) ->  Self {
                inline_closure!($func)
            }
        }
    )* };
}

impl_trait! {
    usize u8 u16 u32 u64 u128:
    |val| val as Self
}

impl_trait! {
    String:
    |val| format!("{}", val)
}

assert_eq!(u8::create(1i32), 1);
assert_eq!(String::create(123), "123");
```
