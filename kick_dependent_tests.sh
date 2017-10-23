#!/bin/bash

### NOTE. Needs TRAVIS_API_TOKEN and GITHUB_API_TOKEN

set -ex

REFERRER_COMMIT="$TRAVIS_COMMIT"
if [ -n "$TRAVIS_PULL_REQUEST_SHA" ]; then
    REFERRER_COMMIT="$TRAVIS_PULL_REQUEST_SHA"
fi

before_script=$(cat <<-END
    export BDCS_PR=$TRAVIS_PULL_REQUEST;                  \
    curl -s -X POST                                       \
        -H \\"User-Agent: weldr team CI\\"                \
        -H \\"Authorization: token \$GITHUB_API_TOKEN\\"  \
        -d '{                                             \
                \\"state\\": \\"pending\\",               \
                \\"target_url\\": \\"'https://travis-ci.org/\$TRAVIS_REPO_SLUG/builds/\$TRAVIS_BUILD_ID'\\",  \
                \\"description\\": \\"Job '\$TRAVIS_JOB_NUMBER' for '\$TRAVIS_REPO_SLUG' has been started\\", \
                \\"context\\": \\"'\$TRAVIS_REPO_SLUG' '\$TRAVIS_JOB_NUMBER' '\$COMMAND'\\"     \
            }'                                            \
        https://api.github.com/repos/$TRAVIS_REPO_SLUG/statuses/$REFERRER_COMMIT
END
)

after_script=$(cat <<-END
    test_result=\\"failure\\";                            \
    if [ \$TRAVIS_TEST_RESULT == 0 ]; then                \
        test_result=\\"success\\";                        \
    fi;                                                   \
    curl -s -X POST                                       \
        -H \\"User-Agent: weldr team CI\\"                \
        -H \\"Authorization: token \$GITHUB_API_TOKEN\\"  \
        -d '{                                             \
                \\"state\\": \\"'\$test_result'\\",         \
                \\"target_url\\": \\"'https://travis-ci.org/\$TRAVIS_REPO_SLUG/builds/\$TRAVIS_BUILD_ID'\\", \
                \\"description\\": \\"Job '\$TRAVIS_JOB_NUMBER' for '\$TRAVIS_REPO_SLUG' has finished\\",    \
                \\"context\\": \\"'\$TRAVIS_REPO_SLUG' '\$TRAVIS_JOB_NUMBER' '\$COMMAND'\\"     \
            }'                                            \
        https://api.github.com/repos/$TRAVIS_REPO_SLUG/statuses/$REFERRER_COMMIT
END
)

body='{
    "request": {
        "message": "Build started by '"$TRAVIS_REPO_SLUG"' PR #'"$TRAVIS_PULL_REQUEST"'",
        "branch":"master",
        "config": {
            "before_script": "'"$before_script"'",
            "after_script": "'"$after_script"'"
        }
    }
}'


# trigger new builds for welder-web and bdcs-cli using the PR artifacts for testing
for target_repo in welder-web bdcs-cli; do
    curl -s -X POST                                 \
        -H "Content-Type: application/json"         \
        -H "Accept: application/json"               \
        -H "Travis-API-Version: 3"                  \
        -H "Authorization: token $TRAVIS_API_TOKEN" \
        -d "$body"                                  \
        https://api.travis-ci.org/repo/weldr%2F$target_repo/requests
done
