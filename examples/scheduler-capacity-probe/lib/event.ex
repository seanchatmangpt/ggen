defmodule SchedulerCapacityProbe.Event do
  @enforce_keys [:cycle, :tool, :operation, :status]
  defstruct [:cycle, :timestamp, :tool, :operation, :status, :durable_bytes, :files, :commit_sha, :next]

  def new(attrs) when is_map(attrs), do: struct!(__MODULE__, attrs)

  def durable?(%__MODULE__{commit_sha: sha}) when is_binary(sha) and byte_size(sha) > 0, do: true
  def durable?(_), do: false

  def outcome(%__MODULE__{status: status}) when status in [:SUCCESS, :REFUSED, :ERROR, :THROTTLED, :TRUNCATED, :UNKNOWN],
    do: status
end
