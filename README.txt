This library defines a syntax extension that allows the use of
user-defined string conversion functions in format strings (that is,
strings passed to printf, sprintf, etc.).

Basic Usage
===========

The basic usage is as follows:

    printf !"The time is %{Time} and the timezone is %{Time.Zone}." time zone

will be transformed by the preprocessor into:

    printf "The time is %s and the timezone is %s."
      (Time.to_string time) (Time.Zone.to_string zone)

In general, specifiers like "%{<Module-path>}" are transformed to "%s",
and [Module-path.to_string] is applied to the appropriate argument.
The module path can even be empty, in which case the generated code calls
[to_string].

Note that you have to prepend the format string with a '!', so that
the preprocessor knows to operate on it.

Sexps
=====

The syntax "%{sexp:<type>}" is also supported.  For example:

    printf !"The time is %{sexp:Time.t}." time

is transformed to:

    printf "The time is %s."
      (Sexplib.Sexp.to_string_hum (<:sexp_of< Time.t >> time)))

This supports arbritrary type expressions.
     
Ksprintf
========

Things like:

    ksprintf f !"Time time is %{Time}" time

work as expected.  In general, it doesn't matter where the format
string occurs in the argument list, although the preprocessor does
assume that the arguments following the format string correspond to
the specifiers of the format string.
     
Partial application
===================

Partial application is supported.  For example:

   printf !"The time is %{Time}"

is essentially transformed to:

  (fun x -> printf "The time is %s" (Time.to_string x))

Actually, it's transformed to something like:

  let y = printf in (fun x -> y "The time is %s" (Time.to_string x))

The reason is to preserve behavior of any side effects.  For example:

   (side_effect (); printf) !"The time is %{Time}"

is transformed to:

  let y = (side_effect (); printf) in
  (fun x -> y "The time is %s" (Time.to_string x))

so that [side_effect ()] is run immediately, just as it would be with
a regular format specifier.
    
Using functions other than [M.to_string]
========================================

The format specifier "%{<Module-path>.<lowercase_identifier>}" corresponds
to a function "<Module-path>.Format.<lowercase_identifier>".  So, for
example:

    printf !"The date is %{Core.Std.Date.iso8601_basic}" date

is transformed to:

    printf "The date is %s" (Core.Std.Date.Format.iso8601_basic date)

Subformats disallowed
=====================

In a regular format string, you can use format specifiers of the form
"%{<spec>%}" and "%(<spec>%)" where <spec> is another format
specifier.

Using these specifiers is disallowed in format strings that are
processed with custom-printf.
