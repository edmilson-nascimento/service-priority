# service-priority
 Lista de manutenção de prioridades de atendimento

![Static Badge](https://img.shields.io/badge/development-abap-blue)
![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/t/edmilson-nascimento/service-priority)
![Static Badge](https://img.shields.io/badge/thiago_barcellos-abap-green)


[![NPM](https://img.shields.io/npm/v/mermaid)](https://www.npmjs.com/package/mermaid)
[![Build CI Status](https://github.com/mermaid-js/mermaid/actions/workflows/build.yml/badge.svg)](https://github.com/mermaid-js/mermaid/actions/workflows/build.yml)
[![npm minified gzipped bundle size](https://img.shields.io/bundlephobia/minzip/mermaid)](https://bundlephobia.com/package/mermaid)
[![Coverage Status](https://codecov.io/github/mermaid-js/mermaid/branch/develop/graph/badge.svg)](https://app.codecov.io/github/mermaid-js/mermaid/tree/develop)
[![CDN Status](https://img.shields.io/jsdelivr/npm/hm/mermaid)](https://www.jsdelivr.com/package/npm/mermaid)
[![NPM Downloads](https://img.shields.io/npm/dm/mermaid)](https://www.npmjs.com/package/mermaid)
[![Join our Discord!](https://img.shields.io/static/v1?message=join%20chat&color=9cf&logo=discord&label=discord)](https://discord.gg/AgrbSrBer3)
[![Twitter Follow](https://img.shields.io/badge/Social-mermaidjs__-blue?style=social&logo=X)](https://twitter.com/mermaidjs_)
[![Covered by Argos Visual Testing](https://argos-ci.com/badge.svg)](https://argos-ci.com?utm_source=mermaid&utm_campaign=oss)
[![OpenSSF Scorecard](https://api.securityscorecards.dev/projects/github.com/mermaid-js/mermaid/badge)](https://securityscorecards.dev/viewer/?uri=github.com/mermaid-js/mermaid)


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


```mermaid
erDiagram
    ZCUSTOMERS {
        CHAR(10) CUSTOMER_ID PK
        VARCHAR(100) NAME
        VARCHAR(100) EMAIL
        TIMESTAMP CREATED_AT
    }
    ZORDERS {
        CHAR(10) ORDER_ID PK
        CHAR(10) CUSTOMER_ID FK
        DATE ORDER_DATE
        DECIMAL(10) TOTAL_AMOUNT
    }
    ZCUSTOMERS ||--o{ ZORDERS : has

```


## Cenários
Aqui a lista com detalhes de alguns cenarios a considerar durante testes.


| Field        | Type         | Key | Description           |
|-------------|-------------|-----|----------------------|
| CUSTOMER_ID | CHAR(10)     | PK  | Unique Customer ID   |
| NAME        | VARCHAR(100) |     | Customer Name        |
| EMAIL       | VARCHAR(100) |     | Customer Email       |
| CREATED_AT  | TIMESTAMP    |     | Record Creation Date |

## Não selecionar ABAP no POP
Verificar erro

## Quantidade de itens atendidos
Mostrar na opção de titulo, a quantidade de itens que estão sendo geridos para prioridades

## Alteração de BC do incidente
No caso de um atendimento ter sido iniciado por uma pessoa e depois passar para outra

## Table: ZCUSTOMERS 📋

| Field        | Type         | Key | Description           |
|-------------|-------------|-----|----------------------|
| CUSTOMER_ID | CHAR(10)     | PK  | Unique Customer ID   |
| NAME        | VARCHAR(100) |     | Customer Name        |
| EMAIL       | VARCHAR(100) |     | Customer Email       |
| CREATED_AT  | TIMESTAMP    |     | Record Creation Date |

## Notas 🗒️
- Usar a função `REUSE_ALV_COMMENTARY_WRITE` para criar um _header_ como é feito no report `ZLOG_RLS10020` para o ALV List;