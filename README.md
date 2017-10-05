### BitBar hacker news 插件

BitBar自带的插件 **[hacker_news.1m.rb](https://github.com/matryer/bitbar-plugins/blob/master/Web/HackerNews/hacker_news.1m.rb)**, 使用过程中有一些问题：

- [ ] 不支持代理，中国大陆访问**hacker_news**很慢
- [ ] 有时候点击的时候会报错，不能链接到目标网页（url的问题）

😋 于是用Haskell仿写了一个，初衷是方便自己使用。
😌 默认代理为 localhost:1111 ，可随意修改。
