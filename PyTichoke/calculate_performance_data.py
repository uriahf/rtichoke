import numpy as np
import pandas as pd
from sklearn.metrics import confusion_matrix
from tqdm import tqdm




def check_probs(probs):    
    """
    Validate probs by ensuring all values are between [0, 1]

    Args:
        probs (np.array): an array of probabilities

    Raises:
        Exception: when validation fails

    Returns:
        Boolean: True when validation passed (else raise exception)
    """
    if min(probs) < 0 or max(probs) > 1:    
        raise Exception('Probs must be within [0, 1]')        
    return True
    
def check_probs_vs_reals(probs, reals):    
    """
    Validate probs vs. reals:
    1. probs and reals must have the same shape
    2. at least two values should be included in each array

    Args:
        probs (np.array): an array of probabilities
        reals (np.array): an array of true values (0's or 1's)

    Raises:
        Exception: when either validation fails

    Returns:
        Boolean: True when validation passed (else raise exception)
    """
    if probs.shape != reals.shape:        
        raise Exception(f'Probs and reals shapes are inconsistent ({probs.shape} and {reals.shape})')        
    elif len(probs) < 2:        
        raise Exception('At least two entries should be included reals and probs')
    return True
    
def check_reals(reals): 
    """
    Validate reals consist of only 0's and 1's, including positive and negative examples

    Args:
        reals (np.array): an array of true values (0's or 1's)

    Raises:
        Exception: when validation fails

    Returns:
        Boolean: True when validation passed (else raise exception)
    """
    if set(reals) != {0, 1}:        
        raise Exception(f'Reals must include only 0\'s and 1\'s')
    return True          
    
def check_by(by):
    """
    Validate `by` argument is between 0 and 0.5

    Args:
        by (float): argument to set the distance between explored threshold probabilities

    Raises:
        Exception: when validation fails

    Returns:
        Boolean: True when validation passed (else raise exception)
    """
    if not (isinstance(by, float) and (by > 0) and (by <= 0.5)):        
        raise Exception(f'Argument `by` must be a float,  0 > by <= 0.5')
    return True
    
def check_stratified_by_input(stratified_by):
    """Validate `stratified_by` argument

    Args:
        stratified_by (string): must be either "probability_threshold" or "ppcr"

    Raises:
        Exception: when validation fails

    Returns:
        Boolean: True when validation passed (else raise exception)
    """
    if stratified_by not in (['probability_threshold', 'ppcr']):        
        raise Exception(f'Argument stratified_by has to be either probability_threshold or ppcr')
    return True
    
def validate_inputs(probs, reals, by, stratified_by):    
    """A mother-function to run all other validation functions

    Args:
        probs (np.array): an array of probabilities
        reals (np.array): an array of true values (0's or 1's)
        by (float): argument to set the distance between explored threshold probabilities
        stratified_by (string): must be either "probability_threshold" or "ppcr"
    """
    check_probs(probs)
    check_probs_vs_reals(probs, reals)
    check_reals(reals)
    check_by(by)
    check_stratified_by_input(stratified_by)    
    

def prepare_performance_data_under_the_hood(probs, reals, by=0.01, stratified_by="probability_threshold", pop_name='pop1'):
    """Generate performance table for a single set of probs and reals.

    Args:
        probs (list, np.array, pd.Series): an array of probabilities
        reals (list, np.array, pd.Series): an array of true values (0's or 1's)
        by (float, optional): argument to set the distance between explored threshold probabilities. Defaults to 0.01.
        stratified_by (string, optional): must be either "probability_threshold" or "ppcr". Defaults to "probability_threshold".        
        pop_name (str, optional): A population name, when asking for performance metrics for several populations. Defaults to 'pop1'.

    Returns:
        pd.DataFrame: a dataframe with performance metrics
    """
    
    # convert inputs to np.arrays
    probs = np.array(probs)
    reals = np.array(reals)
    
    # verify inputs
    validate_inputs(probs, reals, by, stratified_by)
    
    # define probabilty thresholds
    prob_thresholds = np.append(np.arange(0, 1, by),  1) 
    
    # if ppcr is required, adjust probability threholds accordingly.
    if stratified_by == 'ppcr':
        prob_thresholds = np.array([np.quantile(probs, p) for p in prob_thresholds])
        prob_thresholds[0] = 0.0
        
    
    # define performance table
    performance_table = {
        'Population': [], 'probability_threshold': prob_thresholds, 'ppcr': [], 'predicted_positives': [],
        'TP':[], 'FP': [], 'FN': [], 'TN': []
    }

    # run over all probability thresholds and calculate confusion matrix
    for p in tqdm(prob_thresholds, desc='Calculating performance data', leave=False, delay=0.5):
        preds = (probs > p).astype(int)
        performance_table['ppcr'].append(preds.mean())
        performance_table['predicted_positives'].append(preds.sum())

        tn, fp, fn, tp = confusion_matrix(reals ,preds).ravel()     
        performance_table['TP'].append(tp)
        performance_table['FP'].append(fp)
        performance_table['FN'].append(fn)
        performance_table['TN'].append(tn)

    # define additional metrics
    performance_table['Population'] = [pop_name] * len(prob_thresholds)
    performance_table = pd.DataFrame(performance_table)
    performance_table['Sensitivity'] = performance_table['TP'] / (performance_table['TP'] + performance_table['FN'])
    performance_table['Specificity'] = performance_table['TN'] / (performance_table['TN'] + performance_table['FP'])
    performance_table['FPR'] = 1 - performance_table['Specificity']

    performance_table['PPV'] = performance_table['TP'] / (performance_table['TP'] + performance_table['FP'])
    performance_table['NPV'] = performance_table['TN'] / (performance_table['TN'] + performance_table['FN'])
    performance_table['lift'] = performance_table['PPV'] / reals.mean()
    performance_table['Net_benefit'] = (
        performance_table['Sensitivity'] * reals.mean() - 
        (1 - performance_table['Specificity']) * (1 - reals.mean()) * 
        (performance_table['probability_threshold'] / (1 - performance_table['probability_threshold']))
    )    
    return performance_table if stratified_by == 'probability_threshold' else performance_table.iloc[::-1]


def prepare_performance_data(probs, reals, by=0.01, stratified_by="probability_threshold"):
    """
    User's function to produce performance data table for probs/reals. 
    probs/reals may represent one probs vs. one reals, several probs vs. one real, or several probs vs. several reals.

    Args:
        probs (list, np.array, pd.Series, or dict): an array of probabilities or a dictionary {'pop_name': array of probabilities}
        reals (list, np.array, pd.Series, or dict): an array of binary results or a dictionary {'pop_name': arary of binary results}
        by (float, optional): argument to set the distance between explored threshold probabilities. Defaults to 0.01.
        stratified_by (string, optional): must be either "probability_threshold" or "ppcr". Defaults to "probability_threshold".        

    Returns:
        pd.DataFrame: a dataframe with performance metrics
    """
    if isinstance(probs, dict) and isinstance(reals, dict):
        assert probs.keys() == reals.keys(), "When sending dictionaries, probs and reals must have the same keys"
        
        return pd.concat([prepare_performance_data_under_the_hood(probs[key], reals[key], by, stratified_by, pop_name=key) for key in probs.keys()])
    
    if isinstance(probs, dict) and isinstance(reals, (list, np.ndarray, pd.Series)):
        return pd.concat([prepare_performance_data_under_the_hood(probs[key], reals, by, stratified_by, pop_name=key) for key in probs.keys()])
        
    if isinstance(probs, (list, np.ndarray, pd.Series)) and isinstance(reals, (list, np.ndarray, pd.Series)):
        return prepare_performance_data_under_the_hood(probs, reals, by, stratified_by)
    raise Exception(f'Wrong inputs provided for probs and reals')        
