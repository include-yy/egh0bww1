def my_decorator(func):
    print('�b������c��')
    return func

@my_decorator
def my_func():
    print('���b�ʽ����')

my_func()
