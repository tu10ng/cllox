define i32 @main() {
%1 = alloca i32
store i32 1, ptr %1
%2 = alloca i32
store i32 2, ptr %2
%3 = load i32, ptr %1
%4 = load i32, ptr %2
%5 = sub nsw i32 %3, %4
ret i32 %5
}

