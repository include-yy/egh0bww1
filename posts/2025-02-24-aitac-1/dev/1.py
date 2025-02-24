def make_partition(X):
    elements = set() # 集合中的元素
    for x, y in X:
        # 由于自反性，X 中必存在 (x, x)
        elements.add(x)
    visited = set()
    partition = []
    for x in elements:
        if x not in visited:
            # 找到所有和当前 x 等价的元素
            eq_elements = {y for y in elements if (x, y) in X or (y, x) in X}
            # 添加等价子集
            partition.append(eq_elements)
            # 标记等价元素
            visited.update(eq_elements)
    return partition

A = {("a","a"),("b", "b"),("c", "c"),("d", "d"),
     ("a", "b"),("b", "a"),("b", "c"),("c", "b"),
     ("c", "a"),("a", "c")}

res = make_partition(A)

print(res)
# [{'d'}, {'c', 'b', 'a'}]

def is_function(S, T, X):
    is_injection = True
    is_surjective = True

    source_set = set()
    target_set = set()

    for x, y in X:
        if y in target_set:
            is_injection = False
        if x in source_set:
            return "存在一对多映射"
        target_set.add(y)
        source_set.add(x)
    if source_set != S:
        return "原集合不匹配"
    if not target_set.issubset(T):
        return "目标集合不匹配"
    if target_set != T:
        is_surjective = False
    if is_surjective and is_injection:
        return "双射"
    elif is_injection:
        return "单射"
    elif is_surjective:
        return "满射"
    else:
        return "普通函数"
