#!/bin/sh

# Edit these values to your needs:
remote_instance=https://builds.robur.coop
local_user_pass=test:test
local_instance="http://${local_user_pass}@localhost:3000"
global_limit=100
per_job_limit=3  # Limit to 3 builds per job

curl_json () {
	curl --silent --fail --location --header "Accept: application/json" "$@"
}

curl_json "${remote_instance}/" | jq -r .jobs[] | {
    while read -r job_name; do
        build_count=0
        curl_json "${remote_instance}/job/${job_name}" | jq -r .builds[].uuid | {
            while read -r build_uuid; do
                if [ "$global_limit" -eq 0 ] || [ "$build_count" -ge "$per_job_limit" ]; then
                    break 2
                fi
                dest=$(mktemp "builder-${build_uuid}.XXXXXXXXXX")
                curl --silent --fail "${remote_instance}/job/${job_name}/build/${build_uuid}/exec" > "$dest" && {
                    echo "Uploading $job_name build $build_uuid"
                    curl --data-binary "@${dest}" "${local_instance}/upload"
                }
                rm -f "$dest"
                global_limit=$((global_limit - 1))
                build_count=$((build_count + 1))
            done
        }
    done
}
