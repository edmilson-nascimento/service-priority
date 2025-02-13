# service-priority
 Lista de manutenção de prioridades de atendimento

![Static Badge](https://img.shields.io/badge/development-abap-blue)
![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/t/edmilson-nascimento/service-priority)
![Static Badge](https://img.shields.io/badge/thiago_barcellos-abap-green)

Para atender a necessidade, a tabela criada ficaria da seguinte forma.

## Tecnologias usadas
PAra atender a necessidade, o desenvolvimento será feito em SAP usando linguagem ABAP.

Dentro dos recursos que linguagem nos fornece, vamos usar o framework ALV com as possibilidades de "drag-Drop" para fazer a interação com usuário;

A regra de negocio é que, para um incidente, sempre que for feita uma alteração de valores, os campos de 'ultima atualização' vão ser atualizados e também o texto descritivo.
Ao recuperar a informação pro incidente, vamos ter todo o histórico mas exibir apenas o mais recente que é o dado vigente


```mermaid
gantt
    title Solution Development Timeline
    dateFormat  YYYY-MM-DD
    section Analysis
    Define Problem            :done, 2025-01-10, 2025-01-15
    Collect Requirements      :done, 2025-01-16, 2025-01-20
    section Development
    Design Solution           :active, 2025-01-21, 2025-01-25
    Implement Code            :2025-01-26, 2025-02-05
    Unit Testing              :2025-02-06, 2025-02-10
    section Review & Release
    Code Review               :2025-02-11, 2025-02-12
    QA & Testing              :2025-02-13, 2025-02-15
    Deployment                :2025-02-16, 2025-02-17
```


```mermaid

%% Vertical Timeline Workaround using a Sequence Diagram
sequenceDiagram
    participant Start
    participant Analysis
    participant Development
    participant Review
    participant Release
    Start->>Analysis: Define Problem (2025-01-10)
    Analysis->>Analysis: Collect Requirements (2025-01-16)
    Analysis->>Development: Design Solution (2025-01-21)
    Development->>Development: Implement Code (2025-01-26)
    Development->>Development: Unit Testing (2025-02-06)
    Development->>Review: Code Review (2025-02-11)
    Review->>Review: QA & Testing (2025-02-13)
    Review->>Release: Deployment (2025-02-16)

```

```mermaid
gitGraph:
    commit "Ashish"
    branch newbranch
    checkout newbranch
    commit id:"1111"
    commit tag:"test"
    checkout main
    commit type: HIGHLIGHT
    commit
    merge newbranch
    commit
    branch b2
    commit

```