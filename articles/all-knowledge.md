---
title: How could you filter through (most) human knowledge? (a vague idea)
tags: Recommendation Networks, Learning, Fun
priority: 30
date-published: 5/9/2020
date-updated: 4/21/2022
---



How could you filter through (most) human knowledge? (a vague idea)

note: still a work in progress 

**What problem is this idea meant to solve?**

Human knowledge is terribly vast. What you see every day is a tiny, tiny, tiny subset of that. How could you find the best of the best of *all* human knowledge and not just of the tiny subset you’re exposed to? 

**What constitutes the best of the best information?** 

Best of the best would be information that would have the most [utilitarian impact](http://en.wikipedia.org/wiki/Utilitarianism): it would create the most long-term benefits for your life. 

This could be random information you happen upon by chance (such as learning about [stoicism ](http://en.wikipedia.org/wiki/Stoicism)while having no prior interest in philosophy) or information that [coherently ](http://supermemo.guru/wiki/Curing_DSPS_and_insomnia)contributes to problem solving (like learning about [how to fix DSPS ](https://supermemo.guru/wiki/Curing_DSPS_and_insomnia)when wondering just what’s so wrong with your sleep).

I’ll focus on random life changing information first since it’s easier (probably) to solve.  

**How do you find it?**

The obvious answer is to search but as a human you are time constrained and there is no way you could browse even a fraction of all human writings. Even if you had a summary of every writing ever made, it would still take too much time to find the very best with plain reading. 

**How could you reduce the search space?**

Ideal case would be to clone yourself to a computer and evaluate every possible piece of knowledge[kvn] based on your current brain state. That’s not feasible though, so instead you could:

-read articles from authors that seem to consistently put out really good information

-read articles from sites that aggregate information for you

Those do not narrow down the search space enough. Lots of authors have too many articles for you to be able to reasonably skim them all for the best information. Agreggation sites are dependent on people, so they won’t be able to evaluate even a fraction of all possible knowledge. 

Neither will be tailored to the information most complementing your current brain state either[link?]. 

**How could you narrow the search space further & how could you choose information complementing current brain state?**

To narrow the search space, you need to use computers. Computers can evaluate lots of stuff. The problem is the latter questionp

The previous options were constrained by depending on human brains. We need to use *computers*. The problem is that deciding if an article is really the best of the best requires a brain: your brain. No one else will know what the best knowledge for *you* is. 

Thus: we need to figure out how to make a copy of your brain that can valuate things as only a computer can. It does not have to be perfect, it just has to be better. If it’s even 5% better than depending on knowledge aggreggation sites or reading the books from the best of authors, that benefit will compound over time. I’m fairly certain you could do much better than 5% though. 

**How could computers recommend you the best of the best information (imperfectly)?**

Your brain valuates things like this:

new info -> brain processes -> outputs valuation

in brain processes, your brain will valuate the new info based on knowledge you already have and the current brain state. Me when I’m 5 and me when I’m 20 will valuate [meditations ](http://classics.mit.edu/Antoninus/meditations.html)differently based on our differing levels of literacy and understanding. 

Unfortunately, there is no good way to be able to mimic the brain process itself since we can’t upload what we know or the way we value things to a computer.

There is something we can do though.

If I take 100 articles and valuate them based on how life changing they are, a portion of that could be potentially extrapolated to valuate other knowledge. 

We could do one step better than that though; we could create a system that takes the following data:

-my characteristics (interests, age, culture, etc.)

-my valuations of the life changing-ness of articles (maybe on a 1-100 scale*)

-how I traversed link (e.g. if after looking at [propiniquity ](http://en.wikipedia.org/wiki/Propinquity)I investigated the [mere-exposure effect](http://en.wikipedia.org/wiki/Mere-exposure_effect))

from multiple people, and then use that data to extrapolate on highly distance pieces of knowledge. 

For example, if I’ve only rated articles on philosophy or technology, it will be hard for the system to recommend me [Harry Potter and the Methods of Rationality](http://hpmor.com/) (great book btw). But if it can see that people that liked similar things to me also ended up having HPMOR rank highly in life changing-ness then it can recommend that to me too.

This system would work for **random** life changing information, likely a great deal better than depending on authors or knowlede agregation sites like [sivv.io](http://https/www.sivv.io/) since it could process an infinite amount of information given enough time, processing power, and data. 

But, 

**How would you recommend the best of the best information for solving a problem?**

If I am considering the question: how do you fix schooling (which I am) how would I find the best information for solving it? 

I would simply ask people who know or who have investigated the same thing. They are very likely to have highly useful knowledge. 

But what if you don’t want to go around pestering people. How would you do this with a computer?

I don’t think there’s a way to do full recommendations for knowledge for problem solving but I think you could go part way:

if people find article x life-changing, see what article y’s they found complementarily beneficial. As an example, if I read about [semantic distances](http://supermemo.guru/wiki/Semantic_distance), I might mark [Bloom’s 2 Sigma Problem](http://en.wikipedia.org/wiki/Bloom's_2_sigma_problem) as something which gives my brain a “eureka” feeling. 

By keeping track of all these relations that generate “eureka” feelings, you could make it a bit easier to solve problems by giving better suggestions of complementary articles. It’s not perfect but I think it could still be useful, especially if people also give in data of what problems they’re trying to solve. 

[the following is work in progress. it is a mess. please ignore it] 

**How could you** **actually** **build this system?** 

I’m not sure yet. One thing I think could be interesting is the question of incentives for giving ratings.  

**How could you do incentives?** 

You need lots of ratings and data on link traversals for the system to work. One way to get people to give up that data is: you need to put in your data for the system to be accurate for *you*. If you have lots of other people’s data on valuations, but not your own you’ll be stuck with a sort-of useful filter system but not a truly useful filter system that can reflect your mind’s computational state. Putting in your own data is needed for the system to see who you match against 

what if people vote for their own article and do a sybil attack to make themselves look cool? you could prevent this with what I think is called federation: you could trust me and I could trust 5 other people. You could hold those people as highly trustworthy. You could hold the people that I trust’s people who they trust as trustworthyh and useful to check valuations of as being trustwrothy-ish 

*I’m wondering now if the rating scale impacts ratings e.g. will 1-10 vs 1-100 vs 1-1-1000 create significantly different rating skews? People aren’t rational so it’s likely 