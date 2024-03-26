class ThomasSolver:
    def __init__(self, a, b, c, d):
        """
        初始化Thomas算法求解器。
        :param a: 下对角线元素的列表或数组，长度为 n-1。
        :param b: 主对角线元素的列表或数组，长度为 n。
        :param c: 上对角线元素的列表或数组，长度为 n-1。
        :param d: 等式右侧向量的列表或数组，长度为 n。
        """
        self.a = a
        self.b = b
        self.c = c
        self.d = d

    def solve(self):
        """
        执行Thomas算法求解三对角线性方程组。
        :return: 方程组的解向量。
        """
        n = len(self.d)
        # 复制列表以避免修改原始数据
        a, b, c, d = self.a[:], self.b[:], self.c[:], self.d[:]
        
        # 前向消元
        for i in range(1, n):
            w = a[i-1] / b[i-1]
            b[i] -= w * c[i-1]
            d[i] -= w * d[i-1]
        
        # 后向替换
        x = [0] * n
        x[-1] = d[-1] / b[-1]
        for i in range(n-2, -1, -1):
            x[i] = (d[i] - c[i] * x[i+1]) / b[i]
        
        return x