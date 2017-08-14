This is just a fork.  Please see the
[official repository](https://github.com/byorgey/boltzmann).

Boltzmann sampling for random generation of algebraic data types in Haskell.

For background, see:

* [Random binary trees with a size-limited critical Boltzmann sampler](https://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/)
* [Boltzmann sampling for generic Arbitrary instances](https://byorgey.wordpress.com/2016/03/23/boltzmann-sampling-for-generic-arbitrary-instances/)
* Duchon, Philippe, et al. [Boltzmann samplers for the random generation of combinatorial structures.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.217.7672&rep=rep1&type=pdf) Combinatorics, Probability and Computing 13.4-5 (2004): 577-625
* Canou and Darrasse, [Fast and sound random generation for automated testing and benchmarking in Objective Caml](http://web.mit.edu/~ezyang/Public/p61-canou.pdf). Proceedings of the 2009 ACM SIGPLAN workshop on ML. ACM, 2009.

Code in the `gen` directory was originally written by Alexis Darrasse.


Addendum, modifications from official repo
------------------------------------------

For now, this fork adds the following functions:

* `sampleFitness  :: Data a => Int -> (a -> Double) -> Oracle -> [Double]`;
* `oracleToList   :: Oracle -> [(String,Double)]` and
* `oracleFromList :: [(String,Double)] -> Oracle`.

The function `sampleFitness` can be used to sample fitness values.

*Example:* Consider the following (quite artificial) definition of fitness:

	fitnessLength :: Prog -> Double
	fitnessLength = fromIntegral . length . show

where fitness is defined as the length of a program in characters.

To sample fitness values for the default Oracle, just:

	> sampleFitness 10 fitnessLength (toOracle (undefined :: Prog))
	[470.0,61.0,106.0,579.0,99.0,26.0,78099.0,119.0,15.0,37.0,249.0]

To inspect the values for the oracle (deterministic):

	> toOracle (undefined :: Prog)
	[("Dec",1.27),("Exp",0.65),("Id",0.79),("Prog",0.47),("Stat",0.94),("Type",0.78)]

(Use oracleToList in the result of toOracle to just extract the list).

To sample fitness values for a customized Oracle, just:

	> sampleFitness 10 fitnessLength (oracleFromList [("Dec",1.27)
                                                     ,("Exp",0.64)
                                                     ,("Id",0.78)
                                                     ,("Prog",0.47)
                                                     ,("Stat",0.94)
                                                     ,("Type",0.79)])
    [178.0,41.0,34.0,1042.0,15.0,109.0,1093.0,26.0,115.0,44.0,103.0]
