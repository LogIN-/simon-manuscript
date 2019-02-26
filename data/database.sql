-- phpMyAdmin SQL Dump
-- version 4.5.4.1deb2ubuntu2.1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Feb 26, 2019 at 05:40 PM
-- Server version: 10.2.22-MariaDB-10.2.22+maria~xenial-log
-- PHP Version: 7.0.33-0ubuntu0.16.04.1

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `syslog`
--

-- --------------------------------------------------------

--
-- Table structure for table `feature_sets`
--

CREATE TABLE `feature_sets` (
  `id` int(11) NOT NULL,
  `features_hash` char(32) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `processing_id` int(11) DEFAULT NULL,
  `data_source` varchar(25) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `raw_donors_count` int(11) DEFAULT NULL,
  `features` int(11) DEFAULT NULL,
  `training_h_p` float DEFAULT NULL,
  `training_h_no` float DEFAULT NULL,
  `training_l_p` float DEFAULT NULL,
  `training_l_no` float DEFAULT NULL,
  `testing_h_p` float DEFAULT NULL,
  `testing_h_no` float DEFAULT NULL,
  `testing_l_p` float DEFAULT NULL,
  `testing_l_no` float DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `model_details`
--

CREATE TABLE `model_details` (
  `id` int(11) NOT NULL,
  `uid` int(11) DEFAULT NULL,
  `fs_id` int(11) DEFAULT NULL,
  `status` tinyint(1) NOT NULL DEFAULT 0,
  `step` tinyint(4) NOT NULL,
  `method` varchar(25) COLLATE utf8mb4_unicode_ci NOT NULL,
  `p_accuracy` decimal(13,2) DEFAULT NULL,
  `p_kappa` decimal(13,2) DEFAULT NULL,
  `p_auc` decimal(13,2) DEFAULT NULL,
  `p_sensitivity` float DEFAULT NULL,
  `p_specificity` float DEFAULT NULL,
  `t_accuracy` decimal(13,2) DEFAULT NULL,
  `t_kappa` decimal(13,2) DEFAULT NULL,
  `t_auc` decimal(13,2) DEFAULT NULL,
  `t_sensitivity` float DEFAULT NULL,
  `t_specificity` float DEFAULT NULL,
  `positive_ctr` varchar(191) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `time` decimal(13,2) DEFAULT NULL COMMENT 'Time in seconds',
  `features_hash_var_imp` char(32) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `timestamp` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `model_features`
--

CREATE TABLE `model_features` (
  `id` int(11) NOT NULL,
  `fs_id` int(11) DEFAULT NULL,
  `feature_name` varchar(50) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `split_difference_median` decimal(13,2) DEFAULT NULL,
  `indataset_occurance` decimal(13,2) DEFAULT NULL,
  `score_mean` decimal(13,2) DEFAULT NULL,
  `rank_mean` decimal(13,2) DEFAULT NULL,
  `split_difference_mean` decimal(13,2) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- --------------------------------------------------------

--
-- Table structure for table `model_variables`
--

CREATE TABLE `model_variables` (
  `id` int(11) NOT NULL,
  `md_id` int(11) DEFAULT NULL,
  `uid` int(11) DEFAULT NULL,
  `feature_name` varchar(50) COLLATE utf8mb4_unicode_ci NOT NULL,
  `score_perc` decimal(13,2) DEFAULT NULL,
  `score_no` decimal(13,2) DEFAULT NULL,
  `rank` int(11) DEFAULT NULL,
  `timestamp` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `feature_sets`
--
ALTER TABLE `feature_sets`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `features_hash` (`features_hash`,`processing_id`),
  ADD KEY `processing_id_idx` (`processing_id`) USING BTREE;

--
-- Indexes for table `model_details`
--
ALTER TABLE `model_details`
  ADD PRIMARY KEY (`id`),
  ADD KEY `type` (`method`),
  ADD KEY `accuracy` (`p_accuracy`),
  ADD KEY `fs_id` (`fs_id`);

--
-- Indexes for table `model_features`
--
ALTER TABLE `model_features`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `feature_dataset_idx` (`fs_id`,`feature_name`) USING BTREE,
  ADD KEY `feature_name` (`feature_name`);

--
-- Indexes for table `model_variables`
--
ALTER TABLE `model_variables`
  ADD PRIMARY KEY (`id`),
  ADD UNIQUE KEY `md_id_idx` (`md_id`,`feature_name`) USING BTREE,
  ADD KEY `md_id` (`md_id`),
  ADD KEY `feature_name` (`feature_name`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `feature_sets`
--
ALTER TABLE `feature_sets`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=655;
--
-- AUTO_INCREMENT for table `model_details`
--
ALTER TABLE `model_details`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=23809;
--
-- AUTO_INCREMENT for table `model_features`
--
ALTER TABLE `model_features`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=20858;
--
-- AUTO_INCREMENT for table `model_variables`
--
ALTER TABLE `model_variables`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=237502;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
