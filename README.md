# NetworksVizInference

A repository for all the things relating to visual inference and model diagnostics on network models. 

## Outline

1. Motivation and Visual inference Intro
2. Define "visual" effects 
3. Model Diagnostics - what we want to do
4. Set up and describe online experiments 
5. Discuss and analyze experiment results
6. Draw conclusions

Other notes: 

- need a lot of data 
- need a lot of significant models
- currently working on an R package to pull this all together. This might mean another repo -- sorry.

## "Chapter 2"

1. Start with senate data (probably need to remove HRC): higher threshold than .25 for WPC to make network smaller
2. Find the significant effects of the SAOM eval effects
3. Fit various models many times - simple, one interaction, one interaction + one other, two interaction (party, bills), two interaction + one other, etc, ones I think make sense
4. Get mean parameters from each of those models to use in simulation in the future. 
5. Simulate "data" from a bunch of different models: 
    a. The models we care about from step 3
    b. Same models from (a) but with various changes to the parameters. Like What? 
        i. make rate parameters constant over all periods
        ii. double / halve / increase by 50% / etc various parameters one-at-a-time.
        iii. Switch signs of parameters
6. Do I need to refit the model to the data in between getting this "data" and simulating for the lineup? (I think maybe I do)
7. Once I have 100 (?) data sets from each model in 4, create a lineup of each of those data sets among even more sims (M = 12) for use in an mturk study
8. "Easy Lineups" Idea: put the data from the one model among a bunch of sims from "opposite" models (e.g. parameter value switched signs, doubled, etc.) or models with totally different parameters 
9. Novel Lineup Idea (?): store data from all waves, view 6-10 rows of dynamic networks, all waves. 
