if [ ! -z "$TRAVIS" ]
then
    SERVICE_NAME="travis-ci"
    JOB_ID="$TRAVIS_JOB_ID"
else
    echo "Unsupported CI service."
    exit 1;
fi

JSON_FILE="$SERVICE_NAME-$JOB_ID.json"

hpc-coveralls $SERVICE_NAME $JOB_ID $*
