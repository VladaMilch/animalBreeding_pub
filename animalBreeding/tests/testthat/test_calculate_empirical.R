require(testthat)

test_that(desc = "empirical calculation correct",{
    expect_equal(
        calculate_needed_breedings_empirical(condifence_p = 0.9, 
                                             effective_fertility_p = 1, 
                offsprings_n_sample = rpois(n = 10^3, lambda = 7), 
                n_needed = 8)
    )
})