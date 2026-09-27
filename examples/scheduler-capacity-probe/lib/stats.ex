defmodule SchedulerCapacityProbe.Stats do
  @statuses ~w(SUCCESS REFUSED ERROR THROTTLED TRUNCATED UNKNOWN)a

  def summarize(entries) when is_list(entries) do
    entries
    |> Enum.group_by(&Map.get(&1, :tool, :unknown))
    |> Map.new(fn {tool, rows} ->
      {tool, %{operations: length(rows), statuses: status_counts(rows), highest_cycle: highest_cycle(rows)}}
    end)
  end

  defp status_counts(rows) do
    Enum.reduce(rows, %{}, fn row, acc ->
      status = Map.get(row, :status, :UNKNOWN)
      status = if status in @statuses, do: status, else: :UNKNOWN
      Map.update(acc, status, 1, &(&1 + 1))
    end)
  end

  defp highest_cycle(rows), do: rows |> Enum.map(&Map.get(&1, :cycle, 0)) |> Enum.max(fn -> 0 end)
end
