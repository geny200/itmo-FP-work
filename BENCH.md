# [Содержание](README.md)

* [**Домашнее задание 1**](HW1.md)
* [**Домашнее задание 2**](HW2.md)
* [**Домашнее задание 3**](HW3.md)
* **Домашнее задание 4**
    * [**Задание**](HW4.md)
    * [**Реализация**](./src/Hw4)
    * [**Бенчмарки (Задание 1 - 3)**](./bench/Hw4)
    * **Результаты бенчмарков**
---

# Результаты бенчмарков

## Задание 1 - Геометрия
```
benchmarking Task1. perimeter - Naive/10^7
time                 504.5 ms   (485.7 ms .. NaN s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 491.8 ms   (487.6 ms .. 497.4 ms)
std dev              6.310 ms   (706.4 μs .. 8.037 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Task1. perimeter - Fust/10^7
time                 388.7 ms   (365.6 ms .. 422.0 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 394.9 ms   (390.7 ms .. 402.7 ms)
std dev              7.499 ms   (801.7 μs .. 9.217 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Task1. doubleArea - Naive/10^7
time                 440.4 ms   (429.8 ms .. 449.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 444.4 ms   (442.0 ms .. 446.0 ms)
std dev              2.341 ms   (1.032 ms .. 3.239 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Task1. doubleArea - Fust/10^7
time                 372.4 ms   (364.8 ms .. 376.7 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 369.2 ms   (366.8 ms .. 370.3 ms)
std dev              1.880 ms   (660.4 μs .. 2.519 ms)
variance introduced by outliers: 19% (moderately inflated)

```
## Задание 2 - Интегрируемся
```
benchmarking Task2. MonteCarlo - Line /10^6
time                 83.13 ms   (82.31 ms .. 83.99 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 82.42 ms   (82.15 ms .. 82.69 ms)
std dev              471.2 μs   (329.0 μs .. 623.2 μs)

benchmarking Task2. MonteCarlo - Parallel /10^6
time                 43.35 ms   (42.49 ms .. 44.60 ms)
                     0.996 R²   (0.988 R² .. 0.999 R²)
mean                 44.62 ms   (43.61 ms .. 46.37 ms)
std dev              2.707 ms   (1.817 ms .. 4.097 ms)
variance introduced by outliers: 20% (moderately inflated)
```
## Задание 3 - Хэш таблица
```
benchmarking Task3. CHT - Parallel perfom /10^3
time                 5.886 ms   (5.791 ms .. 5.994 ms)
                     0.993 R²   (0.986 R² .. 0.998 R²)
mean                 6.304 ms   (6.192 ms .. 6.472 ms)
std dev              357.7 μs   (286.3 μs .. 463.9 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking Task3. CHT - Parallel perfom /10^4
time                 87.69 ms   (86.68 ms .. 88.96 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 84.68 ms   (82.58 ms .. 85.83 ms)
std dev              2.589 ms   (1.153 ms .. 3.545 ms)

benchmarking Task3. CHT - Parallel consumer - producer/10^5 operations
time                 993.8 ms   (808.3 ms .. 1.221 s)
                     0.991 R²   (0.990 R² .. 1.000 R²)
mean                 1.015 s    (960.9 ms .. 1.075 s)
std dev              71.96 ms   (339.3 μs .. 87.88 ms)
variance introduced by outliers: 20% (moderately inflated)
```