defmodule Assertion do
    defmacro assert({op, _, [lhs, rhs]}) do
        quote do
            Assertion.__perform__(unquote(op), unquote(lhs), unquote(rhs))
        end
    end

    def __perform__(:==, lhs, rhs) when lhs == rhs do
        IO.write "."
    end

    def __perform__(:==, lhs, rhs) do
        IO.puts """
        Falure:
            expected: #{inspect lhs}
            to equal: #{inspect rhs}
            """
    end
end
