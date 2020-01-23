# Latte language compiler
Compiler of Java-Like language (called Latte) to LLVM IR code.

### Syntax
Below some syntax examples.
Full grammar can be found in `/src/Latte.cf` in BNFC format.

```java
// print positive even numbers up to 10

int main () {
  int i = 1 ;
  while (i < 10){
    if (i % 2 == 0) printInt(i) ; 
    i++ ;
  }
  printInt(i) ;
  return 0 ;
}
```


```java

class list {
  int elem;
  list next;
}

// outputs "50 100"
int main() {
  printInt(length(fromTo(1,50)));
  printInt(length2(fromTo(1,100)));
  return 0;
}

int head (list xs) {
  return xs . elem;
}
 
list cons (int x, list xs) {
  list n;
  n = new list;
  n.elem = x;
  n.next = xs;
  return n;
}

int length (list xs) {
  if (xs==(list)null)
    return 0;
  else
    return 1 + length (xs.next);
}

list fromTo (int m, int n) {
  if (m>n)
    return (list)null;
  else 
    return cons (m,fromTo (m+1,n));
}


```


### Optimizations
* Code generated in SSA form
* Global strings reusage
* Removal of unreachable blocks of code
* PHI Nodes insertion instead of memory allocation


### Generated code

``` java

int f(int iters) {
    int a = 1;
    while(iters > 0) {
        a++;
        iters--;
    }
    return a;
}

int main() {
    printInt(f(5));
    return 0;
}

```

For above code `latc_llvm` program generates IR presented below:

``` llvm
define i32@f(i32 %R.0) {
L.1:
    br label %L.2
L.2:
    %R.3 = phi i32 [1, %L.1], [%R.7, %L.6]
    %R.4 = phi i32 [%R.0, %L.1], [%R.8, %L.6]
    %R.5 = icmp sgt i32 %R.4, 0
    br i1 %R.5, label %L.6, label %L.9
L.6:
    %R.7 = add i32 %R.3, 1
    %R.8 = add i32 %R.4, -1
    br label %L.2
L.9:
    ret i32 %R.3
}

define i32@main() {
L.0:
    %R.1 = call i32 @f(i32 5)
    call void @printInt(i32 %R.1)
    ret i32 0
}
```


### Build

```
make all
```


### Usage

After build phase, `latc` binary file will be created.
There is bash script 'latc_llvm'
Let `./full/path/test.lat` be file containing Latte source code.

run

```bash
./latc_llvm full/path/test.lat
```

It will create and link LLVM IR and bitcode

```
full/path/test.ll
full/path/test.bc
```
