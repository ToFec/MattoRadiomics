import numpy as np

def classBalancedSplit(patIdsPerPatient, outcomePerPatient, train_val_ratio):
  patIds = list(dict.fromkeys(patIdsPerPatient))
  patIdCount = np.asarray([patIdsPerPatient.count(f) for f in patIds])
  idxs = np.arange(len(patIds))
  np.random.shuffle(idxs)
  patIdsShuffled = [patIds[i] for i in idxs]
  padIdCountShuffled = patIdCount[idxs]
  
  num_subjects = len(patIdsPerPatient)
  num_train_subjects = int(round(num_subjects * train_val_ratio))
  
  outcomeClassCount = np.unique(outcomePerPatient, return_counts=True)[1]
  outcomeClassWeight = 1.0 - (outcomeClassCount / outcomeClassCount.sum())
  classWeightPerPatient = outcomeClassWeight[outcomePerPatient]
  
  diffToNumTrainSubjects = num_train_subjects
  diffToClassWeights = 1.0 * outcomeClassCount.size
  trainIds = []
  lesionsAdded = 0
  outcomeClassCountRunning = np.zeros(outcomeClassWeight.size)
  patientAdded = False
  i = 0
  npOutcomePerPatient = np.array(outcomePerPatient)
  while True:
    currPatId = patIdsShuffled[i]
    
    numberOfLesions = padIdCountShuffled[i]
    tmplesionsAdded = lesionsAdded + numberOfLesions
    if abs(tmplesionsAdded - num_train_subjects) < diffToNumTrainSubjects:
      outcomePerPatId = npOutcomePerPatient[[currPatId == f for f in patIdsPerPatient]]
      [tmpoutcomeClassPerPatId, tmpoutcomeClassCountPerPatId] = np.unique(outcomePerPatId, return_counts=True)
      outcomeClassCountPerPatId = np.zeros(outcomeClassWeight.size)
      outcomeClassCountPerPatId[tmpoutcomeClassPerPatId] = tmpoutcomeClassCountPerPatId
      
      tmpOutcomeClassCount = outcomeClassCountRunning + outcomeClassCountPerPatId
      
      tmpOutcomeClassWeight = 1.0 - (tmpOutcomeClassCount / tmpOutcomeClassCount.sum())
      
      diffOfClassRationToCohort = np.sum(np.abs(tmpOutcomeClassWeight - outcomeClassWeight))
      if diffOfClassRationToCohort < diffToClassWeights:
        patientAdded = True
        lesionsAdded = lesionsAdded + numberOfLesions
        trainIds.append(currPatId)
        diffToNumTrainSubjects = abs(lesionsAdded - num_train_subjects)
        diffToClassWeights = diffOfClassRationToCohort
        outcomeClassCountRunning = outcomeClassCountRunning + outcomeClassCountPerPatId
        patIdsShuffled = np.delete(patIdsShuffled,i)
        padIdCountShuffled = np.delete(padIdCountShuffled, i)
        i = i - 1
    i = i + 1
    if i == len(patIdsShuffled):
      if patientAdded:
        i = 0
        patientAdded = False
      else:
        break
  ##mit der oberen schleife besteht die gefahr, dass nicht genug faelle in das training set kommen
  ## weil irgendwann das klassen-verhaeltnis nur mehr schlechter wird; darum laufen wir noch einmal ueber
  ## die daten und fuellen das trainings-set mit den cases auf, die das klassenverhaeltnis am wenigsten verschlechtern
  if lesionsAdded < num_train_subjects:
    i = 0
    bestI = 0
    bestDiffToClassWeights = 1.0 * outcomeClassCount.size
    bestOutcomeClassCountPerPatId = None
    while True:
      
      currPatId = patIdsShuffled[i]
      
      outcomePerPatId = npOutcomePerPatient[[currPatId == f for f in patIdsPerPatient]]
      [tmpoutcomeClassPerPatId, tmpoutcomeClassCountPerPatId] = np.unique(outcomePerPatId, return_counts=True)
      outcomeClassCountPerPatId = np.zeros(outcomeClassWeight.size)
      outcomeClassCountPerPatId[tmpoutcomeClassPerPatId] = tmpoutcomeClassCountPerPatId
      
      tmpOutcomeClassCount = outcomeClassCountRunning + outcomeClassCountPerPatId
      
      tmpOutcomeClassWeight = 1.0 - (tmpOutcomeClassCount / tmpOutcomeClassCount.sum())
      
      diffOfClassRationToCohort = np.sum(np.abs(tmpOutcomeClassWeight - outcomeClassWeight))
      if diffOfClassRationToCohort < bestDiffToClassWeights:
        bestI = i
        bestDiffToClassWeights = diffOfClassRationToCohort
        bestOutcomeClassCountPerPatId = outcomeClassCountPerPatId

      i = i + 1
      if i == len(patIdsShuffled):
        
        numberOfLesions = padIdCountShuffled[bestI]
        tmplesionsAdded = lesionsAdded + numberOfLesions
        if abs(tmplesionsAdded - num_train_subjects) < diffToNumTrainSubjects:
          currPatId = patIdsShuffled[bestI]
          lesionsAdded = lesionsAdded + numberOfLesions
          trainIds.append(currPatId)
          diffToNumTrainSubjects = abs(lesionsAdded - num_train_subjects)
          
          outcomeClassCountRunning = outcomeClassCountRunning + bestOutcomeClassCountPerPatId
          patIdsShuffled = np.delete(patIdsShuffled,bestI)
          padIdCountShuffled = np.delete(padIdCountShuffled, bestI)
          i = 0
          bestI = 0
          bestDiffToClassWeights = 1.0 * outcomeClassCount.size
          bestOutcomeClassCountPerPatId = None
        else:
          break        
  
  train_subjects = []
  train_subjects_weights = []
  val_subjects = []
  val_subjects_weights = []
  for (idx, patId) in enumerate(patIdsPerPatient):
    if patId in trainIds:
      train_subjects.append(idx)
      train_subjects_weights.append(classWeightPerPatient[idx])
    else:
      val_subjects.append(idx)
      val_subjects_weights.append(classWeightPerPatient[idx])

  return train_subjects, train_subjects_weights, val_subjects, val_subjects_weights
