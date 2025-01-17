# Build configurations
BUILD_TYPES=("" "release")
COMPILERS=("nim" "zig")
ABI_TYPES=("" "gcc" "gnu")

function runBuild() {
    local cc=$1
    local abi=$2
    local build_type=$3

    # Set environment variables if needed
    local env_vars=""
    [[ "$cc" == "zig" ]] && env_vars="NIM_CC=zig"
    [[ -n "$abi" ]] && env_vars="$env_vars NIM_ABI=$abi"

    # Run build command
    if [[ -z "$build_type" ]]; then
        eval "$env_vars nim Build"
    else
        eval "$env_vars nim Build $build_type"
    fi

    # List build results
    if [ -d "builds/targets" ]; then
        ls -lah builds/targets
    else
        echo "Directory builds/targets does not exist."
    fi
}

# Initial cleanup build
nim CBuild

# Run all build combinations
for cc in "${COMPILERS[@]}"; do
    for abi in "${ABI_TYPES[@]}"; do
        # Skip ABI settings for native nim compiler
        [[ "$cc" == "nim" && -n "$abi" ]] && continue

        for build_type in "${BUILD_TYPES[@]}"; do
            echo "=== Building with CC=$cc ABI=$abi TYPE=$build_type ==="
            runBuild "$cc" "$abi" "$build_type"
            echo "=== Build completed ==="
            echo
        done
    done
done

OUTPUT_CACHE_DIR="builds/tmp/buildAll_cache"

function resetCache() {
    rm -rf "${OUTPUT_CACHE_DIR}"
    mkdir -p "${OUTPUT_CACHE_DIR}"
}

function getCachedOutput() {
    local cmd="$1"
    local hash=$(echo "$cmd" | md5sum | cut -d' ' -f1)
    local cache_file="${OUTPUT_CACHE_DIR}/${hash}"

    if [ -f "$cache_file" ]; then
        cat "$cache_file"
    else
        mkdir -p "${OUTPUT_CACHE_DIR}"
        local output=$($cmd)
        echo "$output" >"$cache_file"
        echo "$output"
    fi
}

function checkOutput() {
    local cmd1=$1
    local cmd2=$2
    local output1=$(getCachedOutput "$cmd1")
    local output2=$(getCachedOutput "$cmd2")

    if [ "$output1" == "$output2" ]; then
        echo "The output of the commands are the same"
    else
        echo "The output of the commands are different"
    fi
}

function checkBuildOutputs() {
    echo "=== Checking build outputs ==="
    local files=(builds/targets/*)
    local n=${#files[@]}

    for ((i = 0; i < n; i++)); do
        for ((j = i + 1; j < n; j++)); do
            if [ -f "${files[i]}" ] && [ -f "${files[j]}" ]; then
                echo "Comparing ${files[i]} with ${files[j]}..."
                cmd1="./${files[i]}"
                cmd2="./${files[j]}"
                checkOutput "$cmd1" "$cmd2"
            fi
        done
    done
    echo "=== Check completed ==="
}

# Initialize cache directory
resetCache

# Run the build process
# ...existing code...

# Check all build outputs
checkBuildOutputs
