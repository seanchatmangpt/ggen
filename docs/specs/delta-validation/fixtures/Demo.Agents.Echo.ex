defmodule Demo.Agents.Echo do
  @moduledoc """
  Generated agent module. Version 1.0.0.
  """

  @doc "Echo handler"
  def handle_message(message) do
    {:ok, message}
  end

  @doc "Init"
  def init(opts) do
    {:ok, %{config: opts}}
  end
end
