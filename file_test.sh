
file=$1

nasm -f elf64 ./tests/$file.s -o ./tests/$file.o
ar rcs ./tests/lib$file.a ./tests/$file.o
rustc -L ./tests/ -lour_code:$file ./runtime/start.rs -o ./tests/$file.run

