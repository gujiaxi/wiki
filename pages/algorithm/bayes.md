# 贝叶斯方法

[gimmick: math]()

> 所谓的贝叶斯方法源于他生前为解决一个“逆概”问题写的一篇文章，而这篇文章是在他死后才由他的一位朋友发表出来的。在贝叶斯写这篇文章之前，人们已经能够计算“正向概率”，如“假设袋子里面有N个白球，M个黑球，你伸手进去摸一把，摸出黑球的概率是多大”。而一个自然而然的问题是反过来：“如果我们事先并不知道袋子里面黑白球的比例，而是闭着眼睛摸出一个（或好几个）球，观察这些取出来的球的颜色之后，那么我们可以就此对袋子里面的黑白球的比例作出什么样的推测”。这个问题，就是所谓的逆概问题。

## 贝叶斯公式

$$
P(A|B)=\frac{P(B|A)P(A)}{P(B)}=\frac{P(B|A)P(A)}{P(B|A)P(A)+P(B|\neg A)P(\neg A)}
$$

## 拼写纠正
比如用户输入`thew`，那么他到底是想输入`the`呢还是`thaw`还是……那个可能性更大，记多个猜测为\\( h_1, h_2 \\)...统一记作\\( \mathbf{h} \\)，他们都属于一个有限且离散的猜测空间\\( H \\)，将用户实际输入的单词记为\\( D \\)。

$$
P(\mathbf{h}|D)=\frac{P(\mathbf{h})P(D|\mathbf{h})}{P(D)}
$$

对于不同的具体猜测\\( h_1, h_2 \\)...\\( P(D) \\)都是一样的，所以在比较的时候可以忽略这个常数。即

$$
P(\mathbf{h}|D) \propto P(\mathbf{h})P(D|\mathbf{h})
$$

这个式子的抽象含义是：对于给定观测数据，一个猜测是好是坏，取决于“这个猜测本身独立的可能性大小（先验概率，Prior）”和“这个猜测生成我们观测到的数据的可能性大小”（似然，Likelihood）的乘积。具体到我们的那个`thew`例子上，含义就是，用户实际是想输入`the`的可能性大小取决于`the`本身在词汇表中被使用的可能性（频繁程度）大小（先验概率）和 想打 the 却打成`thew`的可能性大小（似然）的乘积。

## 奥卡姆剃刀
平面上N个点总是可以用N-1阶多项式来完全拟合，当N个点近似但不精确共线的时候，用 N-1 阶多项式来拟合能够精确通过每一个点，然而用直线来做拟合/线性回归的时候却会使得某些点不能位于直线上。你说到底哪个好呢？多项式？还是直线？一般地说肯定是越低阶的多项式越靠谱（当然前提是也不能忽视“似然”\\( P(D|\mathbf{h}) \\) ，明摆着一个多项式分布您愣是去拿直线拟合也是不靠谱的，这就是为什么要把它们两者乘起来考虑。），原因之一就是低阶多项式更常见，先验概率（\\( P(h) \\)）较大（原因之二则隐藏在\\( P(D|\mathbf{h}) \\)里面），这就是为什么我们要用样条来插值，而不是直接搞一个N-1阶多项式来通过任意 N 个点的原因。

\\( P(D|\mathbf{h}) \\)大不代表你的\\( h \\)（猜测）就是更好的\\( h \\)。还要看\\( h \\)是怎样的。所谓奥卡姆剃刀精神就是说：如果两个理论具有相似的解释力度，那么优先选择那个更简单的（往往也正是更平凡的，更少繁复的，更常见的）。

## 中文分词
1. 南京市/长江大桥

2. 南京/市长/江大桥

这两个分词，到底哪个更靠谱呢？

我们用贝叶斯公式来形式化地描述这个问题，令X为字串（句子），Y为词串（一种特定的分词假设）。我们就是需要寻找使得\\( P(Y|X) \\)最大的Y ，使用一次贝叶斯可得：

$$
P(Y|X) \propto P(Y)P(X|Y)
$$

用自然语言来说就是 这种分词方式（词串）的可能性 乘以 这个词串生成我们的句子的可能性。我们进一步容易看到：可以近似地将\\( P(X|Y) \\)看作是恒等于1的，因为任意假想的一种分词方式之下生成我们的句子总是精准地生成的（只需把分词之间的分界符号扔掉即可）。于是，我们就变成了去最大化\\( P(Y) \\)，也就是寻找一种分词使得这个词串（句子）的概率最大化。

## 统计机器翻译
统计机器翻译的问题可以描述为：给定一个句子e，它的可能的外文翻译f中哪个是最靠谱的。即我们需要计算：\\( P(f|e) \\)：

$$
P(f|e) \propto P(f)P(e|f)
$$

这个式子的右端很容易解释：那些先验概率较高，并且更可能生成句子e的外文句子f将会胜出。

## EM算法
EM的意思是“Expectation-Maximazation”，在这个聚类问题里面，我们是先随便猜一下这两个正态分布的参数：如核心在什么地方，方差是多少。然后计算出每个数据点更可能属于第一个还是第二个正态分布圈，这个是属于Expectation一步。有了每个数据点的归属，我们就可以根据属于第一个分布的数据点来重新评估第一个分布的参数（从蛋再回到鸡），这个是Maximazation。如此往复，直到参数基本不再发生变化为止。这个迭代收敛过程中的贝叶斯方法在第二步，根据数据点求分布的参数上面。

## 最大似然与最小二乘
我们假设直线对于坐标\\( X_i \\)给出的预测\\( f(X_i) \\)是最靠谱的预测，所有纵坐标偏离\\( f(X_i) \\)的那些数据点都含有噪音，是噪音使得它们偏离了完美的一条直线，一个合理的假设就是偏离路线越远的概率越小，具体小多少，可以用一个正态分布曲线来模拟，这个分布曲线以直线对\\( X_i \\)给出的预测\\( f(X_i) \\)为中心，实际纵坐标为 Yi 的点\\( (X_i, Y_i) \\)发生的概率就正比于\\( \mathrm{e}^{-(\Delta Y_i)^2} \\)。

现在我们回到问题的贝叶斯方面，我们要想最大化的后验概率是：

$$
P(h|\mathbf{D}) \propto P(h)P(\mathbf{D}|h)
$$

这里\\( h \\)就是指一条特定的直线，\\( \mathbf{D} \\)就是指这\\( N \\)个数据点。我们需要寻找一条直线\\( h \\)使得\\( P(h)P(\mathbf{D}|h) \\)最大。很显然，\\( P(h) \\)这个先验概率是均匀的，因为哪条直线也不比另一条更优越。所以我们只需要看\\( P(\mathbf{D}|h) \\)这一项，这一项是指这条直线生成这些数据点的概率，刚才说过了，生成数据点\\( (X_i,Y_i) \\)的概率为\\( \mathrm{e}^{-(\Delta Y_i)^2} \\)乘以一个常数。而\\( P(\mathbf{D}|h) = \prod\limits_{i=1}^N P(D_i|h) \\)即假设各个数据点是独立生成的，所以可以把每个概率乘起来。于是生成\\( N \\)个数据点的概率为\\( \prod\limits_{i=1}^N\mathrm{e}^{-(\Delta Y_i)^2}=\mathrm{e}^{-\sum\limits_{i=1}^N(\Delta Y_i)^2} \\)，最大化这个概率就是要最小化\\( \sum\limits_{i=1}^N(\Delta Y_i)^2 \\)

## 朴素贝叶斯方法
条件独立假设正是朴素贝叶斯方法的朴素之处。

## 层级贝叶斯模型
层级贝叶斯模型是现代贝叶斯方法的标志性建筑之一。前面讲的贝叶斯，都是在同一个事物层次上的各个因素之间进行统计推理，然而层次贝叶斯模型在哲学上更深入了一层，将这些因素背后的因素（原因的原因，原因的原因，以此类推）囊括进来。一个教科书例子是：如果你手头有\\( N \\)枚硬币，它们是同一个工厂铸出来的，你把每一枚硬币掷出一个结果，然后基于这\\( N \\)个结果对这\\( N \\)个硬币的\\( \theta \\)（出现正面的比例）进行推理。如果根据最大似然，每个硬币的\\( \theta \\)不是\\( 1 \\)就是\\( \theta \\)，然而我们又知道每个硬币的\\( p(\theta) \\)是有一个先验概率的，也许是一个\\( \beta \\)分布。也就是说，每个硬币的实际投掷结果\\( X_i \\)服从以\\( \theta \\)为中心的正态分布，而\\( \theta \\)又服从另一个以\\( \psi \\)为中心的\\( \beta \\)分布。层层因果关系就体现出来了。进而\\( \psi \\)还可能依赖于因果链上更上层的因素，以此类推。

_来源: <http://mindhacks.cn/2008/09/21/the-magical-bayesian-method/>_