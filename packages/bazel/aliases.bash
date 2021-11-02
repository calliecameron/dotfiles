if [ -z "${PROMPT_COMMAND}" ]; then
    PROMPT_COMMAND='bazel-workspace-check'
else
    PROMPT_COMMAND="${PROMPT_COMMAND}; bazel-workspace-check"
fi
