while True:
    try:
        x = input('Enter the number you want the Fibonaccie Series to be printed to\n: ')
        x = int(x)
        break
    except Exception as err:
        print(err)

fib = []

if x == 1:
    fib = [1]
elif x == 2:
    fib = [1,1]
elif x >=3:
    fib = [1, 1]
    for i in range(2, x):
        fib.append(fib[i-2]+fib[i-1])

print(fib)