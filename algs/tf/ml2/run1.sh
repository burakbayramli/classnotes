export TRAIN_FILE=/home/burak/Documents/ml/ml2/data/adult.data.csv
export EVAL_FILE=/home/burak/Documents/ml/ml2/data/adult.test.csv
export OUTPUT_DIR=/home/burak/Documents/ml/ml2/output
export TRAIN_STEPS=1000
/home/burak/Downloads/google-cloud-sdk/bin/gcloud \
    ml-engine local train --package-path cloudml-samples-master/census/estimator/trainer/ \
    --module-name trainer.task \
    -- \
    --train-files $TRAIN_FILE \
    --eval-files $EVAL_FILE \
    --job-dir $OUTPUT_DIR \
    --train-steps $TRAIN_STEPS \
    --eval-steps 100

