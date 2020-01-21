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


### Build

```
make all
```

