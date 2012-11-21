rm *.hi
rm *.o
find . -maxdepth 1 ! -name "*.*" | xargs rm
