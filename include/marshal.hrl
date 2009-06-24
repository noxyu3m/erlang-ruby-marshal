-define(MARSHAL_MAJOR, 4).
-define(MARSHAL_MINOR, 8).

-define(TYPE_NIL,     $0).
-define(TYPE_TRUE,    $T).
-define(TYPE_FALSE,   $F).

-define(TYPE_FIXNUM,  $i).
-define(TYPE_BIGNUM,  $l).

-define(TYPE_UCLASS,  $C).
-define(TYPE_FLOAT,   $f).
-define(TYPE_STRING,  34).
-define(TYPE_REGEXP,  $/).
-define(TYPE_ARRAY,   $[).
-define(TYPE_HASH,    ${).

-define(TYPE_SYMBOL,  $:).
-define(TYPE_SYMLINK, $;).

-define(TYPE_IVAR,    $I).
-define(TYPE_LINK,    $@).
