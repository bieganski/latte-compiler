source_filename = "tests/good/core015.lat"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

declare i8* @_malloc(i32)
declare i8* @_strcat(i8*, i8*)
declare i32 @_strcmp(i8*, i8*)
declare i8* @_strcpy(i8*, i8*)
declare i32 @_strlen(i8*)
declare void @error()
declare void @printInt(i32)
declare void @printString(i8*)
declare i32 @readInt()
declare i8* @readString()
@str.0 = private unnamed_addr constant [1 x i8] c"\00", align 1

define i32@main() {
%1 = call i32 @ev(i32 17)
call void @printInt(i32 %1)
ret i32 0
}

define i32@ev(i32) {
%2 = icmp sgt i32 %0, 0
%3 = icmp ne i1 %2, 0
br i1 %3, label %4, label %8
; block 4
%5 = sub i32 %0, 2
%6 = call i32 @ev(i32 %5)
ret i32 %6
br label %16
; block 8
%9 = icmp slt i32 %0, 0
%10 = icmp ne i1 %9, 0
br i1 %10, label %11, label %13
; block 11
ret i32 0
br label %15
; block 13
ret i32 1
br label %15
; block 15
br label %16
; block 16
ret i32 0
}
attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}