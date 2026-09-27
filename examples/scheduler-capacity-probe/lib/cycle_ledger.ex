defmodule SchedulerCapacityProbe.CycleLedger do
  def new(run_id), do: %{run_id: run_id, cycles: []}
  def record(ledger, entry), do: %{ledger | cycles: ledger.cycles ++ [entry]}
end
