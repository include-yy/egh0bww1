def my_decorator(func):
    print('裝飾器加點料')
    return func

@my_decorator
def my_func():
    print('被裝飾函式執行')

my_func()
