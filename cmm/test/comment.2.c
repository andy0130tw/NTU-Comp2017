int main() {
    /* some arcane (but legal) comments */

    /*/  /* the comment is the whole line! ... */
    /*/*/  /* ... but not this line */
    /**/ /* empty */
    /***/ /* a star! */
    /****/ /* stars! */
    /*****/ /* even more */

    int xx/* should split the token */yy;
}
