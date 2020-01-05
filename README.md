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

### Optimizations
* Global strings reusage 
* PHI Nodes instead of memory allocation
