#!/bin/sh

# Edit these values to your needs:
remote_instance=https://builds.robur.coop
local_user_pass=test:test
local_instance="http://${local_user_pass}@localhost:3000"
limit=100

curl_json () {
	curl --silent --fail --location --header "Accept: application/json" "$@"
}

curl_json "${remote_instance}/" | jq -r .jobs[] | {
	while read -r job_name; do
		curl_json "${remote_instance}/job/${job_name}" | jq -r .builds[].uuid | {
			while read -r build_uuid; do
				if [ "$limit" -eq 0 ]; then
					break 2;
				fi
				dest=$(mktemp "builder-${build_uuid}.XXXXXXXXXX")
				curl --silent --fail "${remote_instance}/job/${job_name}/build/${build_uuid}/exec" > "$dest" && {
					echo "Uploading $job_name build $build_uuid"
                                        curl --data-binary "@${dest}" "${local_instance}/upload"
				}
				rm -f "$dest"
				limit=$((limit - 1))
			done
		}
	done
}
