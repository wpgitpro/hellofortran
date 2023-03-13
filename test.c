void addnums( int* a, int* b ) 
{
    int c = (*a) + (*b);  /* convert pointers to values, then add them */
    printf("sum of %i and %i is %i\n", (*a), (*b), c );
}