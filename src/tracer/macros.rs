
macro_rules! add_info {
    ($name:ident $return_type:ty{
        $(
            $field_name:ident => {
                $(
                    $variant:pat => $field_value:expr
                ),+
            }
        )+
    } => $wildcard:expr) => {
        impl $name {
            $(
                paste::paste! (
                    fn [<get_$field_name>](value: &$name) -> $return_type {
                        match value {
                            $(
                                $variant => $field_value,
                            )*

                            #[allow(unreachable_patterns)]
                            _ => $wildcard
                        }
                    }
                );
            )+
        }
    };
}

pub(super) use add_info;

macro_rules! add_find_by {
    ($name:ident $return_type:ty {
        $(
            $field_name:ident<$field_type:ty> => {
                $(
                    $field_value:pat => $variant:expr
                ),+
            }
        )+
    } => $wildcard:expr) => {
        impl $name {
            $(
                paste::paste! (
                    fn [<find_by_$field_name>](value: $field_type) -> $return_type {
                        match value {
                            $(
                                $field_value => $variant,
                            )*

                            #[allow(unreachable_patterns)]
                            _ => $wildcard
                        }
                    }
                );
            )+
        }
    };
}
pub(super) use add_find_by;

macro_rules! add_parse_executor {
    ($name:ident ($($arg_name:ident $arg_type:ty),*) $return_type:ty {
        $(
            $variant:pat => $code:block
        ),+
    } => $wildcard:expr) => {
        impl $name {
            pub fn parse_and_run(variant: &$name, $($arg_name: $arg_type,)*) -> $return_type {
                match variant {
                    $(
                        $variant => $code
                    )*
                    _ => $wildcard
                }
            }
        }
    };
}

pub(super) use add_parse_executor;