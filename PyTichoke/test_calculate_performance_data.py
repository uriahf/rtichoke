import unittest
import numpy as np
import pandas as pd
from numpy.testing import assert_array_equal, assert_allclose
from pandas.testing import assert_frame_equal
from calculate_performance_data import *

# UnitTests created with ChatGPT! :-)

class TestCheckProbs(unittest.TestCase):
    def test_valid_probs(self):
        self.assertTrue(check_probs(np.array([0.1, 0.3, 0.6, 0.9])))

    def test_invalid_probs_low(self):
        with self.assertRaises(Exception):
            check_probs(np.array([-0.1, 0.3, 0.6, 0.9]))

    def test_invalid_probs_high(self):
        with self.assertRaises(Exception):
            check_probs(np.array([0.1, 0.3, 1.1, 0.9]))

###

class TestCheckStratifiedByInput(unittest.TestCase):
    def test_valid_stratified_by_probability_threshold(self):
        self.assertTrue(check_stratified_by_input("probability_threshold"))

    def test_valid_stratified_by_ppcr(self):
        self.assertTrue(check_stratified_by_input("ppcr"))

    def test_invalid_stratified_by(self):
        with self.assertRaises(Exception):
            check_stratified_by_input("invalid_stratified_by")

###

class TestCheckProbsVsReals(unittest.TestCase):
    def test_valid_probs_vs_reals(self):
        probs = np.array([0.1, 0.3, 0.6, 0.9])
        reals = np.array([0, 1, 0, 1])
        self.assertTrue(check_probs_vs_reals(probs, reals))

    def test_invalid_shape(self):
        probs = np.array([0.1, 0.3, 0.6, 0.9])
        reals = np.array([0, 1])
        with self.assertRaises(Exception):
            check_probs_vs_reals(probs, reals)

    def test_invalid_len(self):
        probs = np.array([0.1])
        reals = np.array([0])
        with self.assertRaises(Exception):
            check_probs_vs_reals(probs, reals)

class TestCheckReals(unittest.TestCase):
    def test_valid_reals(self):
        # Test a valid input
        reals = np.array([0, 1, 1, 0, 1])
        self.assertTrue(check_reals(reals))

    def test_reals_with_negative_values(self):
        # Test that an exception is raised when reals include negative values
        reals = np.array([-1, 0, 1])
        with self.assertRaises(Exception):
            check_reals(reals)

    def test_reals_with_a_single_value(self):
        # Test that an exception is raised when reals include only positive outcomes
        reals = np.array([1, 1, 1])
        with self.assertRaises(Exception):
            check_reals(reals)

    def test_reals_with_non_binary_values(self):
        # Test that an exception is raised when reals include non-binary values
        reals = np.array([0, 1, 2])
        with self.assertRaises(Exception):
            check_reals(reals)


class TestCheckBy(unittest.TestCase):
    def test_valid_by(self):
        # Test a valid input
        by = 0.25
        self.assertTrue(check_by(by))

    def test_by_not_a_float(self):
        # Test that an exception is raised when the input is not a float
        by = "0.25"
        with self.assertRaises(Exception):
            check_by(by)

    def test_by_out_of_range(self):
        # Test that an exception is raised when the input is out of range
        by = 0.6
        with self.assertRaises(Exception):
            check_by(by)


    def test_check_stratified_by_input(self):
        # test for valid input
        self.assertTrue(check_stratified_by_input("ppcr"))
        
        # test for invalid input
        with self.assertRaises(Exception):
            check_stratified_by_input("random")


class TestValidateInputs(unittest.TestCase):
    def test_validate_inputs(self):
        # test for valid input
        self.assertIsNone(validate_inputs(
            np.array([0.1, 0.5, 0.9]),
            np.array([0, 1, 0]),
            0.1,
            "ppcr"
        ))
        
        # test for invalid input
        with self.assertRaises(Exception):
            validate_inputs(
                np.array([0.1, 0.5]),
                np.array([0, 1, 0]),
                0.1,
                "ppcr"
            )
        with self.assertRaises(Exception):
            validate_inputs(
                np.array([-0.1, 0.5, 1.1]),
                np.array([0, 1, 0]),
                0.1,
                "ppcr"
            )
        with self.assertRaises(Exception):
            validate_inputs(
                np.array([0.1, 0.5, 0.9]),
                np.array([0, 1]),
                0.1,
                "ppcr"
            )
        with self.assertRaises(Exception):
            validate_inputs(
                np.array([0.1, 0.5, 0.9]),
                np.array([0, 1, 0]),
                0,
                "ppcr"
            )
        with self.assertRaises(Exception):
            validate_inputs(
                np.array([0.1, 0.5, 0.9]),
                np.array([0, 1, 0]),
                0.6)
            


class TestPreparePerformanceDataUnderTheHood(unittest.TestCase):
    def setUp(self):
        self.probs = np.array([0.7, 0.8, 0.9, 0.4, 0.2, 0.6, 0.5])
        self.reals = np.array([1, 1, 1, 0, 0, 1, 0])
        self.by = 0.1
        self.stratified_by = "probability_threshold"
        self.pop_name = 'pop1'
        
    def test_prepare_performance_data_returns_dataframe(self):
        result = prepare_performance_data_under_the_hood(self.probs, self.reals)
        self.assertIsInstance(result, pd.DataFrame)
        
    def test_prepare_performance_data_contains_expected_columns(self):
        result = prepare_performance_data_under_the_hood(self.probs, self.reals)
        expected_cols = [
            'Population', 'probability_threshold', 'ppcr', 'predicted_positives',
            'TP', 'FP', 'FN', 'TN', 'Sensitivity', 'Specificity', 'FPR', 'PPV',
            'NPV', 'lift', 'Net_benefit'
        ]
        self.assertCountEqual(result.columns, expected_cols)
        
    def test_prepare_performance_data_contains_correct_prob_thresholds(self):
        result = prepare_performance_data_under_the_hood(self.probs, self.reals, by=self.by)
        expected_prob_thresholds = np.array([0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
        assert_allclose(result['probability_threshold'].values, expected_prob_thresholds)
                
    def test_prepare_performance_data_contains_expected_population_name(self):
        result = prepare_performance_data_under_the_hood(self.probs, self.reals, pop_name=self.pop_name)
        self.assertTrue((result['Population'] == self.pop_name).all())      
      

class TestPreparePerformanceData(unittest.TestCase):    
    def test_single_probas_single_labels(self):
        probas = np.array([0.2, 0.4, 0.6, 0.8])
        labels = np.array([0, 0, 1, 1])
        result = prepare_performance_data(probas, labels)
        self.assertIsInstance(result, pd.DataFrame)
        self.assertEqual(len(result), 101)  # 101 rows: 100 thresholds + 1 row for overall metrics
        self.assertEqual(result['Population'].nunique(), 1)  # 1 population
        
    def test_multiple_probas_single_labels(self):
        probas1 = np.array([0.2, 0.4, 0.6, 0.8])
        probas2 = np.array([0.3, 0.5, 0.7, 0.9])
        labels = np.array([0, 0, 1, 1])
        result = prepare_performance_data({'pop1': probas1, 'pop2': probas2}, labels)
        self.assertIsInstance(result, pd.DataFrame)
        self.assertEqual(len(result), 202)  # 2 populations, 101 thresholds each
        self.assertEqual(result['Population'].nunique(), 2)  # 2 populations
        
    def test_multiple_probas_multiple_labels(self):
        probas1 = np.array([0.2, 0.4, 0.6, 0.8])
        probas2 = np.array([0.3, 0.5, 0.7, 0.9])
        labels1 = np.array([0, 0, 1, 1])
        labels2 = np.array([1, 0, 0, 1])
        result = prepare_performance_data({'pop1': probas1, 'pop2': probas2}, {'pop1': labels1, 'pop2': labels2})
        self.assertIsInstance(result, pd.DataFrame)
        self.assertEqual(len(result), 202)  # 2 populations, 2 label sets, 101 thresholds each
        self.assertEqual(result['Population'].nunique(), 2)  # 2 populations
        
    def test_wrong_inputs(self):
        with self.assertRaises(Exception):
            prepare_performance_data({'pop1': [0.1, 0.2]}, [0, 1, 1])
        with self.assertRaises(Exception):
            prepare_performance_data(1, 2)
        

if __name__ == '__main__':
    unittest.main()