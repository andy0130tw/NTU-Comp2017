int main() {
    /* a string should NOT be delimited by a '\n' character,
       even if this is allowed in ANSI C with escape character */
    write("foo\
    bar");
}
