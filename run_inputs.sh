
for filename in ./input/*.snek; do
    file=$(basename $filename .snek)
    cargo run -- ./input/$file.snek ./input/$file.s
    nasm -f elf64 ./input/$file.s -o ./input/$file.o
    ar rcs ./input/lib$file.a ./input/$file.o
    rustc -L ./input/ -lour_code:$file ./runtime/start.rs -o ./input/$file.run
    cat ./input/$file.snek
    ./input/$file.run
done

rm -f input/*.a input/*.s input/*.run input/*.o
