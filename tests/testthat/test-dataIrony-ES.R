context("dataIrony")


test_that("test_fit_param", {

    set.seed(1213)
    eps <- 1e-3
    N  <- 20
    y1 <- rnorm(N, mean=5)
    y2 <- y1 + 1:N
    
    se1 <- ses(y1)
    a1 <- es_fit(se1)
    
    se2 <- ses(y2)
    a2 <- es_fit(se2)
    
    de1 <- des(y1)
    b1 <- es_fit(de1)
    
    de2 <- des(y2)
    b2 <- es_fit(de2)

    expect_true(abs(a1-0.2511) < eps)
    expect_true(abs(a2-0.9999) < eps)
    expect_true(abs(b1-0.0843) < eps)
    expect_true(abs(b2-0.4043) < eps)    
})


## peds <- build_pedigrees(test_pop, progress = FALSE)
## test_that("build_pedigrees works", {
##   expect_output(print(peds), regexp = "^List of 2 pedigrees \\(of size 12, 7\\)$")
##   expect_equal(pedigrees_count(peds), 2L)
## })

## if (FALSE) {
##   plot(peds[[1]])
##   plot(peds[[2]])
## }

## ped <- peds[[1]]
## pids <- sort(get_pids_in_pedigree(ped))
## test_that("pedigree pids works", {
##   expect_equal(length(pids), 12L)
##   expect_true(all(pids == c(1L:11L, 19L)))
##   expect_equal(length(get_pids_in_pedigree(peds[[2]])), 7L)
## })

## test_that("meiotic_dist works", {
##   expect_equal(0L, meiotic_dist(get_individual(test_pop, pid = 1L), 
##                                 get_individual(test_pop, pid = 1L)))
  
##   expect_equal(1L, meiotic_dist(get_individual(test_pop, pid = 1L), 
##                                 get_individual(test_pop, pid = 6L)))
  
##   expect_equal(4L, meiotic_dist(get_individual(test_pop, pid = 1L), 
##                                 get_individual(test_pop, pid = 10L)))
  
##   expect_equal(-1L, meiotic_dist(get_individual(test_pop, pid = 1L), 
##                                  get_individual(test_pop, pid = 12L)))
  
  
##   expect_equal(2L, meiotic_dist(get_individual(test_pop, pid = 15L), 
##                                 get_individual(test_pop, pid = 16L)))
## })


## LOCI <- 100L
## pedigrees_all_populate_haplotypes(peds, mutation_rates = rep(0L, LOCI), progress = FALSE)
## test_that("pedigrees_all_populate_haplotypes works", {
##   expect_output(print(peds), regexp = "^List of 2 pedigrees \\(of size 12, 7\\)$")
## })

## test_that("get_haplotype_matching_individuals works", {
##   expect_equal(length(indvs), length(get_haplotype_matching_individuals(indvs, rep(FALSE, LOCI))))
##   expect_equal(0L, length(get_haplotype_matching_individuals(indvs, rep(TRUE, LOCI))))
##   expect_equal(lapply(indvs, get_pid), lapply(get_haplotype_matching_individuals(indvs, rep(FALSE, LOCI)), get_pid))
## })

## test_that("count_haplotype_occurrences_individuals works", {
##   expect_equal(19L, count_haplotype_occurrences_individuals(indvs, rep(FALSE, LOCI)))
##   expect_equal(0L, count_haplotype_occurrences_individuals(indvs, rep(TRUE, LOCI)))
  
##   expect_equal(12L, count_haplotype_occurrences_pedigree(ped, rep(FALSE, LOCI)))
##   expect_equal(5L, count_haplotype_occurrences_pedigree(ped, 
##                                                         rep(FALSE, LOCI), 
##                                                         generation_upper_bound_in_result = 0L))
##   expect_equal(5L+4L, count_haplotype_occurrences_pedigree(ped, 
##                                                            rep(FALSE, LOCI), 
##                                                            generation_upper_bound_in_result = 1L))
##   expect_equal(5L+4L+2L, count_haplotype_occurrences_pedigree(ped, 
##                                                               rep(FALSE, LOCI), 
##                                                               generation_upper_bound_in_result = 2L))
## })

## indvs_is_female <- get_individuals_is_female(indvs)
## indv_females <- indvs[indvs_is_female]
## indv_males <- indvs[!indvs_is_female]

## test_that("get_individuals_is_female works", {
##   expect_equal(10L, length(indv_females))
##   expect_equal(9L, length(indv_males))
## })

## suspect <- get_individual(test_pop, pid = 1L)
## match_females <- get_haplotype_matching_individuals(individuals = indv_females, 
##                                                     haplotype = get_haplotype(suspect))
## match_males <- get_haplotype_matching_individuals(individuals = indv_males, 
##                                                   haplotype = get_haplotype(suspect))

## test_that("get_haplotype_matching_individuals works", {
##   expect_equal(length(match_females), length(indv_females))
##   expect_equal(length(match_males), length(indv_males))
## })

## match_info_females <- get_matches_info(suspect = suspect, matching_indv = match_females)
## match_info_males <- get_matches_info(suspect = suspect, matching_indv = match_males)

## mei_res_females <- match_info_females[order(match_info_females[, 3L]), ] # order by pid
## meioses_females <- mei_res_females[, 1L]
## mei_res_males <- match_info_males[order(match_info_males[, 3L]), ] # order by pid
## meioses_males <- mei_res_males[, 1L]

## test_that("get_matches_info works", {
##   expect_equal(mei_res_females[, 3L], c(2L, 6L:11L))
##   expect_true(all(mei_res_females[, 2L] == 0L)) # max L0 == 0
  
##   expect_equal(mei_res_males[, 3L], c(1L, 3L, 4L, 5L, 19L))
##   expect_true(all(mei_res_males[, 2L] == 0L)) # max L0 == 0
  
##   expect_equal(length(match_females), length(indv_females))
##   expect_equal(length(match_males), length(indv_males))
  
##   expect_equal(4L, meiotic_dist(suspect, get_individual(test_pop, pid = 3L))) 
##   expect_equal(meiotic_dist(suspect, get_individual(test_pop, pid = 3L)), 
##                meioses_males[mei_res_males[, 3L] == 3L])
  
##   expect_equal(4L, meiotic_dist(suspect, get_individual(test_pop, pid = 10L))) 
##   expect_equal(meiotic_dist(suspect, get_individual(test_pop, pid = 10L)), 
##                meioses_females[mei_res_females[, 3L] == 10L])
## })


## haps_from_ped <- get_haplotypes_in_pedigree(ped)
## haps_from_pids <- get_haplotypes_pids(test_pop, pids)
## haps_from_indvs <- get_haplotypes_individuals(indvs)
## hap_from_indv <- lapply(pids, function(pid) get_haplotype(get_individual(test_pop, pid)))

## test_that("pedigrees_all_populate_haplotypes haplotypes works", {
##   #haps_from_ped
##   expect_true(is.list(haps_from_ped))
##   expect_equal(length(haps_from_ped), 12L)
##   expect_equal(length(haps_from_ped[[1L]]), LOCI)
##   expect_true(all(unlist(haps_from_ped) == 0L))
  
##   #haps_from_pids
##   expect_true(is.list(haps_from_pids))
##   expect_equal(length(haps_from_pids), 12L)
##   expect_equal(length(haps_from_pids[[1L]]), LOCI)
##   expect_true(all(unlist(haps_from_pids) == 0L))
  
##   #haps_from_indvs
##   expect_equal(nrow(haps_from_indvs), 19L)
##   expect_true(all(unique(unlist(haps_from_indvs))) == FALSE)
##   expect_equal(as.integer(unique(c(haps_from_indvs))), 0L)
  
##   #hap_from_indv
##   expect_equal(haps_from_ped, hap_from_indv)
## })

## no_vars <- unlist(lapply(indvs, get_haplotype_no_variants))
## test_that("get_haplotype_no_variants works", {
##   expect_true(all(no_vars == 0L))
##   expect_equal(no_vars, unlist(apply(haps_from_indvs, 1, sum)))
## })

## f_hap <- c(TRUE, rep(FALSE, LOCI-1L))
## pedigrees_all_populate_haplotypes_custom_founders(peds, 
##                                                   mutation_rates = rep(0, LOCI), 
##                                                   get_founder_haplotype = function() f_hap,
##                                                   progress = FALSE)

## haps_from_ped <- get_haplotypes_in_pedigree(ped)
## haps_from_pids <- get_haplotypes_pids(test_pop, pids)
## haps_from_indvs <- get_haplotypes_individuals(indvs)
## hap_from_indv <- lapply(pids, function(pid) get_haplotype(get_individual(test_pop, pid)))

## test_that("pedigrees_all_populate_haplotypes_custom_founders works", {
##   #haps_from_ped
##   expect_true(is.list(haps_from_ped))
##   expect_equal(length(haps_from_ped), 12L)
##   expect_equal(length(haps_from_ped[[1L]]), LOCI)
##   expect_true(length(unique(unlist(haps_from_ped))) == 2L)
  
##   expect_equal(haps_from_ped, haps_from_pids)
##   expect_equal(haps_from_ped, hap_from_indv)
##   expect_equal(haps_from_indvs, do.call(rbind, lapply(seq_along(indvs), function(j) f_hap)))
## })

## no_vars <- unlist(lapply(indvs, get_haplotype_no_variants))
## test_that("get_haplotype_no_variants works", {
##   expect_true(all(no_vars == 1L))
##   expect_true(all(no_vars == sum(f_hap)))
##   expect_equal(no_vars, unlist(apply(haps_from_indvs, 1, sum)))
## })

## ##########################################################

## # hashmap <- build_haplotypes_hashmap(indvs)
## # 
## # test_that("hashmap works", {
## #   expect_output(print_haplotypes_hashmap(hashmap), "Total = 19", fixed = TRUE)
## #   expect_equal(length(indvs), length(get_haplotype_matching_individuals_from_hashmap(hashmap, f_hap)))
## #   expect_equal(0L, length(get_haplotype_matching_individuals_from_hashmap(hashmap, !f_hap)))
## # })

## test_that("infer_haplotype_ids not called", {
##   expect_error(build_haplotypeids_hashmap(indvs, progress = FALSE))
## })

## infer_haplotype_ids(indvs, progress = FALSE)
## hashmap <- build_haplotypeids_hashmap(indvs, progress = FALSE)
## hap_id <- get_haplotype_id_individual(indvs[[1]])

## test_that("hashmap works", {
##   expect_equal(length(indvs), length(get_haplotypeid_matching_individuals_from_hashmap(hashmap, hap_id)))
##   expect_equal(0L, length(get_haplotypeid_matching_individuals_from_hashmap(hashmap, 1000)))
##   expect_silent(delete_haplotypeids_hashmap(hashmap))
## })




## #######################################################

## test_that("get_generations_from_final", {
##   expect_equal(0L, get_generations_from_final(get_individual(test_pop, 1)))
##   expect_equal(1L, get_generations_from_final(get_individual(test_pop, 6)))
## })

## test_that("print_individual", {
##   expect_output(print_individual(indvs[[1L]]), 
##                 "  pid = 19 [M] in generation 1 with mother pid = 10 and no children",
##                 fixed = TRUE)
## })

## test_that("print_individual", {
##   expect_output(print_individual(get_individual(test_pop, 10)), 
##                 "pid = 10 \\[F\\] in generation 2 with mother pid = 11 and children \\(n = 2\\)")
## })


## test_that("get_pedigree_id_from_pid", {
##   expect_equal(get_pedigree_id(peds[[1]]), 1)
##   expect_equal(get_pedigree_id(peds[[2]]), 2)
## })

## test_that("get_pedigree_from_individual", {
##   expect_equal(get_pedigree_id(get_pedigree_from_individual(get_individual(test_pop, 10))), 
##                get_pedigree_id_from_pid(test_pop, 10))
## })

## test_that("get_pedigree_id_from_pid", {
##   expect_equal(get_pedigree_id_from_pid(test_pop, 1), get_pedigree_id_from_pid(test_pop, 6))
##   expect_equal(get_pedigree_id_from_pid(test_pop, 15), get_pedigree_id_from_pid(test_pop, 16))
## })

## test_that("pedigrees_table", {
##   expect_equal(pedigrees_table(peds), c("7" = 1, "12" = 1))
## })

## test_that("print_pedigree", {
##   expect_output(print_pedigree(peds[[2]]), "Pedigree with 7 individuals")
## })

## test_that("get_is_female_in_pedigree", {
##   expect_equal(sum(get_is_female_in_pedigree(ped = peds[[1]])), 7)
##   expect_equal(sum(!get_is_female_in_pedigree(ped = peds[[1]])), 5)
  
##   expect_equal(sum(get_is_female_in_pedigree(ped = peds[[2]])), 3)
##   expect_equal(sum(!get_is_female_in_pedigree(ped = peds[[2]])), 4)
## })

## peds_tidy <- get_pedigrees_tidy(pedigrees = peds)
## test_that("get_pedigrees_tidy", {
##   expect_equal(unlist(peds_tidy$ped_ids), c(1, 2))
## })




## #######################################################

## meis_dist_0 <- meioses_generation_distribution(individual = get_individual(test_pop, 1), 
##                                                generation_upper_bound_in_result = 0)


## test_that("meioses_generation_distribution gen 0", {
##   expect_true(all(meis_dist_0[, 1L] == 0L))
  
##   # itself
##   expect_equal(meis_dist_0[1L, 2L], c("meioses" = 0L)) 
##   expect_equal(meis_dist_0[1L, 3L], c("count" = 1L))
  
##   # 2 indvs (pid = 2, 3) in dist 4
##   expect_equal(meis_dist_0[2L, 2L], c("meioses" = 4L)) 
##   expect_equal(meis_dist_0[2L, 3L], c("count" = 2L))
  
##   # 2 indvs (pid = 4, 5) in dist 6
##   expect_equal(meis_dist_0[3L, 2L], c("meioses" = 6L)) 
##   expect_equal(meis_dist_0[3L, 3L], c("count" = 2L))
## })


## test_that("pedigree_size_generation gen 0", {
##   # 1 female in generation 0
##   expect_equal(pedigree_size_generation(peds[[1]], TRUE, 0), 1L)
  
##   # 4 males in generation 0
##   expect_equal(pedigree_size_generation(peds[[1]], FALSE, 0), 4L)
  
##   # 7 female in total
##   expect_equal(pedigree_size_generation(peds[[1]], TRUE, -1), 7L)
  
##   # 5 males in total
##   expect_equal(pedigree_size_generation(peds[[1]], FALSE, -1), 5L)
## })

## test_that("population_size_generation gen 0", {
##   # 1 female in generation 0
##   expect_equal(population_size_generation(test_pop, TRUE, 0), 1L)
  
##   # 4+3 males in generation 0
##   expect_equal(population_size_generation(test_pop, FALSE, 0), 4L+3L)
  
##   # 7+3 female in total
##   expect_equal(population_size_generation(test_pop, TRUE, -1), 7L+3L)
  
##   # 5+4 males in total
##   expect_equal(population_size_generation(test_pop, FALSE, -1), 5L+4L)
## })



#######################################################
# destructor
rm(test_pop)
gc()
#######################################################

