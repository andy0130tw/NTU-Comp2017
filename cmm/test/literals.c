int main(void) {
    /* test for integer literals */
    int x;
    x = 1;
    /* the literal should be 1, not -1 */
    x = -1;
    x = 1 + -1;

    /* test for floating literals */
    float fff;
    fff = .;   /* but a dot should not be recognized as a number */
    fff = 0.;
    fff = 1.;  /* omitting trailing decimal places should be accepted */
    fff = 1.0;
    fff = .3;  /* omitting leading 0 should be accepted */
    fff = 0.3;
    fff = 123.34;

    fff = 1e3;  /* digit sequence following "e" should be accepted, this
                   is a common pitfall! */
    fff = 1.0e3;
    fff = .5e3;
    fff = 9.e7;
    fff = 9.2e+7;
    fff = 9.2E+7;
    fff = .25e-3;
    fff = .25E-3;

    /* test for string literals
       it seems that we have no (explicit) pointers here,
       so we wrap it in a function */
    write("");
    write("foo bar");
    write("\n");  /* but it is 2 chars! */
    write("\r");  /* ditto */
    write("\689.2");  /* no, nothing special! */
    write("\\");  /* this should be accepted, but "\"" should not, see fail.str.1 */
    write("\");   /* this should be accepted, even though it's not allowed in ANSI C! */
}
