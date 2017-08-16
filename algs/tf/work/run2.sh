BUCKET_NAME="census-123"
REGION=us-central1
TRAIN_DATA=gs://$BUCKET_NAME/data/adult.data.csv
EVAL_DATA=gs://$BUCKET_NAME/data/adult.test.csv
JOB_NAME=census_single_1
OUTPUT_PATH=gs://$BUCKET_NAME/$JOB_NAME
/home/burak/Downloads/google-cloud-sdk/bin/gcloud  ml-engine jobs submit training $JOB_NAME \
    --job-dir $OUTPUT_PATH \
    --runtime-version 1.2 \
    --module-name trainer.task \
    --package-path cloudml-samples-master/census/estimator/trainer/ \
    --region $REGION \
    -- \
    --train-files $TRAIN_DATA \
    --eval-files $EVAL_DATA \
    --train-steps 1000 \
    --verbosity DEBUG
